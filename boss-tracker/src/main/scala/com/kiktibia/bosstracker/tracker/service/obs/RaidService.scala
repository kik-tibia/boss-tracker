package com.kiktibia.bosstracker.tracker.service.obs

import cats.effect.IO
import cats.implicits.*
import com.kiktibia.bosstracker.tracker.discord.DiscordBot
import com.kiktibia.bosstracker.tracker.repo.BossTrackerRepo
import com.kiktibia.bosstracker.tracker.repo.DiscordMessageDto
import com.kiktibia.bosstracker.tracker.repo.RaidDto
import com.kiktibia.bosstracker.tracker.repo.RaidRow
import com.kiktibia.bosstracker.tracker.service.FileIO
import com.kiktibia.bosstracker.tracker.service.obs.ObsModel.*
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*

import java.time.Duration
import java.time.Instant
import java.time.LocalDate
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.UUID

class RaidService(
    fileIO: FileIO,
    discordBot: DiscordBot,
    repo: BossTrackerRepo
) {

  def checkForRaidUpdates(): IO[Unit] = {
    val raidData: IO[List[Raid]] = fileIO.parseRaidData().map(s => parser.decode[List[Raid]](s)).map(_.getOrElse(Nil))
    for
      raidDataModifiedTime <- fileIO.raidDataModifiedTime()
      data <- raidData
      latestObsRaids = data
        .filter(_.worldName == "Nefera")
        .groupBy(_.raidId)
        .map { case (uuid, raidNotifs) => raidNotifs.max }
        .toList
      updatedRaids <- latestObsRaids
        .map { latestObsRaid =>
          for
            repoRaid <- repo.getRaid(latestObsRaid.raidId)
            didUpdate <- handleUpdate(latestObsRaid, repoRaid, raidDataModifiedTime.atZone(zone))
            maybeUpdatedRaid <- if (didUpdate) repo.getRaid(latestObsRaid.raidId) else IO.pure(None)
          yield maybeUpdatedRaid
        }
        .sequence
        .map(_.flatten)
      _ <-
        if (updatedRaids.nonEmpty) {
          val alerts = RaidPredictor.noteworthyRaidWarnings(updatedRaids)
          val mostRecentSSTime = getMostRecentSSTime()
          for
            raids <- repo.getRaids(latestObsRaids.map(_.raidId))
            raidsWithCandidates <- raids.map { raid =>
              val candidates = (raid.area, raid.subarea, raid.raidType) match {
                case (Some(area), None, None) => repo.getOtherRaidsStage1(area)
                case (Some(area), Some(subarea), None) => repo.getOtherRaidsStage2(area, subarea)
                case _ => IO.pure(Nil)
              }
              candidates.map(c => RaidWithCandidates(raid, c))
            }.sequence
            raidsWithProbabilities = raidsWithCandidates.map(RaidPredictor.calculateProbabilities)
            _ = RaidPredictor.logProbabilities(raidsWithProbabilities)
            embed = discordBot.generateRaidEmbed(raidsWithProbabilities)
            discordMessages <- repo.getDiscordMessages("raids", mostRecentSSTime)
            newMessages <- discordBot.createOrUpdateEmbeds(embed, discordMessages, alerts)
            _ <- repo.upsertDiscordMessages(
              newMessages.map(m =>
                DiscordMessageDto(0, mostRecentSSTime, "raids", m.getGuild().getId(), m.getChannel().getId(), m.getId())
              )
            )
          yield ()
        } else IO.unit
      _ <- updatedRaids.filter(_.raidType.isDefined).map(checkIfMetadataCorrect).sequence
    yield ()
  }

  private def getMostRecentSSTime(): OffsetDateTime = {
    val now = ZonedDateTime.now(zone)
    val t = now.withHour(10).withMinute(0).withSecond(0).withNano(0)
    if (now.isBefore(t)) t.minusDays(1).toOffsetDateTime() else t.toOffsetDateTime()
  }

  private def checkIfMetadataCorrect(raid: RaidDto): IO[Unit] = {
    val raidStart = raid.startDate.toInstant.atZone(zone)
    raid.raidType match {
      case Some(raidType) =>
        for
          maybePreviousRaid <- repo.getPreviousRaidOfSameType(raid.raidId)
          _ <- maybePreviousRaid match {
            case Some(previousRaid) =>
              val previousRaidSS = zdtToSS(previousRaid.startDate.toInstant.atZone(zone)).toLocalDate
              val thisRaidSS = zdtToSS(raidStart).toLocalDate
              val interval = ChronoUnit.DAYS.between(previousRaidSS, thisRaidSS)
              (raidType.windowMin, raidType.windowMax) match {
                case (Some(windowMin), Some(windowMax)) =>
                  if (interval < windowMin || interval > windowMax)
                    discordBot.sendRaidMessage(
                      s"Raid `${raidType.name}` (ID `${raid.raidId}`) occurred after an interval of $interval days. Current window for this raid type is set to $windowMin - $windowMax days. Consider updating the raid type metadata."
                    )
                case _ => ()
              }
              IO.unit
            case None => IO.unit
          }
          nextSS = zdtToSS(raidStart).plusDays(1)
          minsBeforeSS = ChronoUnit.MINUTES.between(raidStart, nextSS)
          _ <-
            if (minsBeforeSS < raidType.duration.getOrElse(0) * 60) {
              discordBot.sendRaidMessage(
                s"Raid `${raidType.name}` (ID `${raid.raidId}`, raid type ID `${raidType.id}`) occurred $minsBeforeSS minutes before SS. Current duration for this raid type is set to ${raidType.duration
                    .getOrElse(1)} hour(s). Consider updating the raid type metadata."
              )
              IO.unit
            } else IO.unit
        yield ()
      case _ => IO.unit
    }
  }

  private def handleUpdate(obsRaid: Raid, maybeRaidDto: Option[RaidDto], modifiedTime: ZonedDateTime): IO[Boolean] = {
    val timeLeftToRaidStartFromObs = ChronoUnit.SECONDS.between(modifiedTime, obsRaid.startDate)

    // If it's less than 14 minutes and 55 seconds to the raid start and it still has no subarea in the OBS response,
    // change the subarea to the string "None", which is then specially handled in the database call to find raids with no subarea
    val obsRaidUpdated =
      if (timeLeftToRaidStartFromObs < (15 * 60 - 5) && obsRaid.subareaName.isEmpty)
        obsRaid.copy(subareaName = Some("None"))
      else obsRaid

    maybeRaidDto match {
      case None =>
        upsertRaid(obsRaidUpdated)
      case Some(raidDto) =>
        if (raidDto.subarea.isEmpty && obsRaidUpdated.subareaName.isDefined) {
          upsertRaid(obsRaidUpdated)
        } else if (raidDto.raidType.isEmpty && obsRaidUpdated.raidTypeId != 0) {
          upsertRaid(obsRaidUpdated)
        } else {
          IO.pure(false)
        }
    }
  }

  private def upsertRaid(obsRaid: Raid): IO[Boolean] = {
    val raidTypeId = if (obsRaid.raidTypeId == 0) None else Some(obsRaid.raidTypeId.toLong)
    repo
      .upsertRaid(
        RaidRow(
          obsRaid.raidId,
          raidTypeId,
          obsRaid.areaName,
          obsRaid.subareaName,
          obsRaid.startDate.toOffsetDateTime()
        )
      )
      .map(_ => true)
  }
}
