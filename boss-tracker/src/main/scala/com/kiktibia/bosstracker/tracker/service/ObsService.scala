package com.kiktibia.bosstracker.tracker.service

import cats.effect.IO
import cats.implicits.*
import com.kiktibia.bosstracker.config.Config
import com.kiktibia.bosstracker.tracker.CirceCodecs
import com.kiktibia.bosstracker.tracker.ObsModel.Raid
import com.kiktibia.bosstracker.tracker.repo.BossTrackerRepo
import com.kiktibia.bosstracker.tracker.service.FileIO
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*

import java.time.LocalDate
import java.time.LocalTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import com.kiktibia.bosstracker.tracker.repo.RaidRow
import java.util.UUID
import java.time.OffsetDateTime
import com.kiktibia.bosstracker.tracker.repo.RaidDto

class ObsService(
    fileIO: FileIO,
    discordBot: DiscordBot,
    repo: BossTrackerRepo
) {

  def checkForMwcUpdate(): IO[Unit] = IO {
    if (shouldPostMwcUpdate()) {
      println("Posting mwc update")
      val timeOfLastMwcChange: ZonedDateTime = fileIO
        .getTimeOfLastMwcChange()

      val mwcTimeString: String = timeOfLastMwcChange
        .withZoneSameInstant(ZoneId.of("Europe/Berlin"))
        .format(DateTimeFormatter.ofPattern("HH:mm:ss z"))
      val mwcTimingMessage: String = s"MWC change detected at `$mwcTimeString`"

      discordBot.sendMwcTiming(mwcTimingMessage)

      val mwcDateString: String = timeOfLastMwcChange
        .withZoneSameInstant(ZoneId.of("Europe/Berlin"))
        .format(DateTimeFormatter.ISO_LOCAL_DATE)
      val mwcDetails: String = fileIO.getMwcDetails()
      val mwcDetailsMessage: String =
        s"**Mini world changes for $mwcDateString:**\n${parseMwcJson(mwcDetails)}"

      discordBot.sendMwcDetails(mwcDetailsMessage)

      fileIO.updateLastMwcPost(timeOfLastMwcChange)
    }
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

  private def shouldPostMwcUpdate(): Boolean = {
    val timeOfLastPost = fileIO.getTimeOfLastMwcPost()
    val timeOfLastMwcChange = fileIO.getTimeOfLastMwcChange()
    timeOfLastMwcChange.isAfter(timeOfLastPost)
  }

  def checkForRaidUpdates(): IO[Unit] = {
    val raidJson: String = fileIO.parseRaidData()
    val maybeParsed = parser.decode[List[Raid]](raidJson)
    val raidData: List[Raid] = maybeParsed.getOrElse(Nil)

    val updates = raidData
      .groupBy(_.raidId)
      .map { case (uuid, raidNotifs) =>
        val latestObsRaid = raidNotifs.maxBy(_.announcementDate)
        for
          repoRaid <- repo.getRaid(uuid)
          didUpdate <- handleUpdate(latestObsRaid, repoRaid)
          _ <-
            if (didUpdate) {
              for
                updatedRepoRaid <- repo.getRaid(uuid)
                _ = discordBot.sendRaidUpdate(latestObsRaid, updatedRepoRaid)
              yield IO.unit
            } else IO.unit
        yield ()
      }
      .toList

    updates.sequence.map(_ => ())
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
