package com.kiktibia.bosstracker.tracker.service

import cats.effect.IO
import cats.implicits.*
import com.kiktibia.bosstracker.config.Config
import com.kiktibia.bosstracker.tracker.CirceCodecs
import com.kiktibia.bosstracker.tracker.ObsModel.Raid
import com.kiktibia.bosstracker.tracker.repo.BossTrackerRepo
import com.kiktibia.bosstracker.tracker.repo.DiscordMessageDto
import com.kiktibia.bosstracker.tracker.repo.RaidDto
import com.kiktibia.bosstracker.tracker.repo.RaidRow
import com.kiktibia.bosstracker.tracker.service.FileIO
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import net.dv8tion.jda.api.EmbedBuilder

import java.time.LocalDate
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

class ObsService(
    fileIO: FileIO,
    discordBot: DiscordBot,
    repo: BossTrackerRepo
) {

  def checkForMwcUpdate(): IO[Unit] = {
    for
      shouldPost <- shouldPostMwcUpdate()
      _ <-
        if (shouldPost) {
          for
            timeOfLastMwcChange <- fileIO.getTimeOfLastMwcChange()
            mwcTimeString = timeOfLastMwcChange
              .withZoneSameInstant(ZoneId.of("Europe/Berlin"))
              .format(DateTimeFormatter.ofPattern("HH:mm:ss z"))
            mwcTimingMessage = s"MWC change detected at `$mwcTimeString`"
            _ = discordBot.sendMwcTiming(mwcTimingMessage)
            mwcDateString = timeOfLastMwcChange
              .withZoneSameInstant(ZoneId.of("Europe/Berlin"))
              .format(DateTimeFormatter.ISO_LOCAL_DATE)
            mwcDetails <- fileIO.getMwcDetails()
            mwcDetailsMessage =
              s"**Mini world changes for $mwcDateString:**\n${parseMwcJson(mwcDetails)}"
            _ = discordBot.sendMwcDetails(mwcDetailsMessage)
            _ <- fileIO.updateLastMwcPost(timeOfLastMwcChange)
          yield ()
        } else IO.unit
    yield ()
  }

  private def parseMwcJson(jsonString: String): String = {
    parse(jsonString)
      .flatMap { json =>
        json.as[List[Json]].flatMap {
          _.traverse { mwc =>
            mwc.hcursor.get[String]("title")
          }
        }
      }
      .getOrElse(List.empty)
      .mkString("\n")
  }

  private def shouldPostMwcUpdate(): IO[Boolean] = {
    for
      timeOfLastPost <- fileIO.getTimeOfLastMwcPost()
      timeOfLastMwcChange <- fileIO.getTimeOfLastMwcChange()
    yield timeOfLastMwcChange.isAfter(timeOfLastPost)
  }

  def checkForRaidUpdates(): IO[Unit] = {
    val raidData: IO[List[Raid]] = fileIO.parseRaidData().map(s => parser.decode[List[Raid]](s)).map(_.getOrElse(Nil))
    for
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
            didUpdate <- handleUpdate(latestObsRaid, repoRaid)
            maybeUpdatedRaid <- if (didUpdate) repo.getRaid(latestObsRaid.raidId) else IO.pure(None)
          yield maybeUpdatedRaid
        }
        .sequence
        .map(_.flatten)
      _ <-
        if (updatedRaids.nonEmpty) {
          val alerts = noteworthyRaidWarnings(updatedRaids)
          val mostRecentSSTime = getMostRecentSSTime()
          for
            raids <- repo.getRaids(latestObsRaids.map(_.raidId))
            embed = discordBot.generateRaidEmbed(raids)
            discordMessages <- repo.getDiscordMessages("raids", mostRecentSSTime)
            newMessages <- discordBot.createOrUpdateEmbeds(embed, discordMessages, alerts)
            _ <- repo.upsertDiscordMessages(
              newMessages.map(m =>
                DiscordMessageDto(0, mostRecentSSTime, "raids", m.getGuild().getId(), m.getChannel().getId(), m.getId())
              )
            )
          yield ()
        } else IO.unit
    yield ()
  }

  private def getMostRecentSSTime(): OffsetDateTime = {
    val now = ZonedDateTime.now(ZoneId.of("Europe/Berlin"))
    val t = now.withHour(10).withMinute(0).withSecond(0).withNano(0)
    if (now.isBefore(t)) t.minusDays(1).toOffsetDateTime() else t.toOffsetDateTime()
  }

  private def noteworthyRaidWarnings(raids: List[RaidDto]): List[String] = {
    raids.flatMap { raid =>
      if (raid.area.contains("edron") && raid.subarea.isEmpty) Some("Edron raid")
      else None
    }
  }

  private def handleUpdate(obsRaid: Raid, maybeRaidDto: Option[RaidDto]): IO[Boolean] = {
    maybeRaidDto match {
      case None =>
        upsertRaid(obsRaid)
      case Some(raidDto) =>
        if (raidDto.subarea.isEmpty && obsRaid.subareaName.isDefined) {
          upsertRaid(obsRaid)
        } else if (raidDto.raidType.isEmpty && obsRaid.raidTypeId != 0) {
          upsertRaid(obsRaid)
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
