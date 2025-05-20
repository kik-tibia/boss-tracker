package com.kiktibia.bosstracker.tracker.service

import cats.effect.IO
import cats.implicits.*
import com.kiktibia.bosstracker.config.Config
import com.kiktibia.bosstracker.tracker.CirceCodecs
import com.kiktibia.bosstracker.tracker.ObsModel.*
import com.kiktibia.bosstracker.tracker.repo.BossTrackerRepo
import com.kiktibia.bosstracker.tracker.repo.DiscordMessageDto
import com.kiktibia.bosstracker.tracker.repo.RaidDto
import com.kiktibia.bosstracker.tracker.repo.RaidRow
import com.kiktibia.bosstracker.tracker.service.FileIO
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import net.dv8tion.jda.api.EmbedBuilder

import java.time.Duration
import java.time.LocalDate
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.UUID

class ObsService(
    fileIO: FileIO,
    discordBot: DiscordBot,
    repo: BossTrackerRepo
) {

  private val zone = ZoneId.of("Europe/Berlin")

  def checkForMwcUpdate(): IO[Unit] = {
    for
      shouldPost <- shouldPostMwcUpdate()
      _ <-
        if (shouldPost) {
          for
            timeOfLastMwcChange <- fileIO.getTimeOfLastMwcChange()
            mwcTimeString = timeOfLastMwcChange
              .withZoneSameInstant(zone)
              .format(DateTimeFormatter.ofPattern("HH:mm:ss z"))
            mwcTimingMessage = s"MWC change detected at `$mwcTimeString`"
            _ = discordBot.sendMwcTiming(mwcTimingMessage)
            mwcDateString = timeOfLastMwcChange
              .withZoneSameInstant(zone)
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
            raidsWithCandidates <- raids.map { raid =>
              val candidates = (raid.area, raid.subarea, raid.raidType) match {
                case (Some(area), None, None) => repo.getOtherRaidsStage1(area)
                case (Some(area), Some(subarea), None) => repo.getOtherRaidsStage2(area, subarea)
                case _ => IO.pure(Nil)
              }
              candidates.map(c => RaidWithCandidates(raid, c))
            }.sequence
            raidsWithProbabilities = raidsWithCandidates.map(calculateProbabilities)
            _ = logProbabilities(raidsWithProbabilities)
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

  private def logProbabilities(raids: List[RaidWithProbabilities]): Unit = {
    println("--- Probabilities ---")
    raids.filter(_.probabilities.nonEmpty).foreach { raid =>
      println(raid.raid.startDate)
      raid.probabilities.sortBy(-_.probability).foreach { p =>
        println(f"${p.probability * 100}%.2f%% - ${p.raidType.name}")
      }
    }
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
                s"Raid `${raidType.name}` (ID `${raid.raidId}`) occurred $minsBeforeSS minutes before SS. Current duration for this raid type is set to ${raidType.duration
                    .getOrElse(1)} hour(s). Consider updating the raid type metadata."
              )
              IO.unit
            } else IO.unit
        yield ()
      case _ => IO.unit
    }
  }

  private def zdtToSS(zdt: ZonedDateTime) = {
    val zdtAt10am = ZonedDateTime.of(zdt.toLocalDate, LocalTime.of(10, 0), zone)
    if (zdt.isBefore(zdtAt10am)) zdtAt10am.minusDays(1) else zdtAt10am
  }

  private def calculateProbabilities(raid: RaidWithCandidates): RaidWithProbabilities = {
    val raidStart: ZonedDateTime = raid.raid.startDate.toInstant.atZone(zone)
    val currentSS: ZonedDateTime = zdtToSS(raidStart)
    val candidatesWithTimeLeft = raid.candidates.flatMap { c =>
      (c.lastOccurrence, c.windowMin, c.windowMax, c.duration, c.eventStart, c.eventEnd) match {
        case (_, _, _, _, Some(eventStart), Some(eventEnd)) if !insideEvent(raidStart, eventStart, eventEnd) => None
        case (_, _, _, Some(duration), _, _) if ChronoUnit.HOURS.between(raidStart, currentSS.plusDays(1)) < duration =>
          None
        case (Some(lastOccurrence), Some(windowMin), Some(windowMax), maybeDuration, _, _) =>
          val lastZdt = lastOccurrence.toInstant.atZone(zone)
          val ssOfLast = zdtToSS(lastZdt)
          val windowStart = ssOfLast.plusDays(windowMin)
          val windowEnd = ssOfLast.plusDays(windowMax + 1)
          if (raidStart.isBefore(windowStart) || raidStart.isAfter(windowEnd))
            None
          else {
            // The integer number of days remaining in the window
            val daysInWindow = ChronoUnit.DAYS.between(currentSS.toLocalDate, windowEnd.toLocalDate)
            // The fraction (0 to 1) into the day that the last raid occurred - e.g. 16:00 = 0.25, 06:00 = 0.8333...
            val lastRaidFractionIntoDay = ChronoUnit.SECONDS.between(ssOfLast, lastZdt) / 86400.0
            // The total number of seconds left in the window across all days, taking into account hours the raid can't occur (close to SS)
            val secondsInWindow = (ChronoUnit.SECONDS.between(raidStart, windowEnd)
              - (daysInWindow * maybeDuration.getOrElse(1) * 3600)).toDouble
            // If a raid occurs shortly after SS, the first day of the window is higher chance than the last day of the window
            // Conversely, if a raid occurs shortly before SS, the last day of the window is higher chance than the first day
            // These chances approach 0% and 100% depending on how close to SS the raid occurred
            // The following two vals (weighted seconds) take this into account
            val weightedEndSecondsInWindow =
              if (currentSS.plusDays(1) == windowEnd) secondsInWindow
              else secondsInWindow - (1 - lastRaidFractionIntoDay) * (86400 - maybeDuration.getOrElse(1) * 3600)
            val weightedStartEndSecondsInWindow =
              if (currentSS == windowStart) weightedEndSecondsInWindow / (1 - lastRaidFractionIntoDay)
              else weightedEndSecondsInWindow

            Some(c, weightedStartEndSecondsInWindow)
          }
        case (None, _, Some(windowMax), maybeDuration, _, _) =>
          Some(c, windowMax.toDouble * (86400 - maybeDuration.getOrElse(1) * 3600))
        case _ => None
      }
    }
    // Now we have the time left, in seconds, for each candidate raid
    // This time is weighted to take into account lower probabilities for the start and end of the window
    // The reciprocal of this time can be thought of as the instantaneous chance that a raid will occur at that exact second
    // So to work out the probability that a raid occurs first out of all other events, we use this formula
    // P_i is the probability that raid i occurs first
    // p_i is the instantanous chance above (i.e. the reciprocal of the weighted window)
    // P_i = p_i / (p_1 + p_2 + ... p_n)
    // And to avoid any issues with floating point precision for small numbers, we just multiply each p_i by a large number (timeSum)
    val timeSum = candidatesWithTimeLeft.map(_._2).sum
    val denominator = candidatesWithTimeLeft.map(c => timeSum / c._2).sum
    val probabilities = candidatesWithTimeLeft
      .map { c =>
        CandidateProbability(c._1, (timeSum / c._2) / denominator)
      }
      .sortBy(-_.probability)
    RaidWithProbabilities(raid.raid, probabilities)
  }

  private def insideEvent(start: ZonedDateTime, eventStart: LocalDate, eventEnd: LocalDate): Boolean = {
    val zonedEventStart = ZonedDateTime.of(eventStart, LocalTime.of(10, 0), zone)
    val zonedEventEnd = ZonedDateTime.of(eventEnd, LocalTime.of(10, 0), zone)
    !start.isBefore(zonedEventStart) && !start.isAfter(zonedEventEnd)
  }

  private def getMostRecentSSTime(): OffsetDateTime = {
    val now = ZonedDateTime.now(zone)
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
