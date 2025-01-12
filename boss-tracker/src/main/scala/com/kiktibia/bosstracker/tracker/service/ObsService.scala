package com.kiktibia.bosstracker.tracker.service

import cats.implicits.*
import com.kiktibia.bosstracker.config.Config
import com.kiktibia.bosstracker.tracker.ObsModel.Raid
import com.kiktibia.bosstracker.tracker.service.FileIO
import io.circe.Error
import io.circe.Json
import io.circe.parser.*

import java.time.LocalDate
import java.time.LocalTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

class ObsService(
    cfg: Config,
    fileIO: FileIO,
    discordBot: DiscordBot
) {

  def handleMwcUpdate(): Unit = {
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
        s"**Mini world changes changes for $mwcDateString:**\n${parseMwcJson(mwcDetails)}"

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

  // def handleRaidsUpdate(): Unit = {
  //   import com.kiktibia.bosstracker.tracker.CirceCodecs
  //   import io.circe.*
  //   import io.circe.generic.auto.*
  //   import io.circe.parser.*
  //
  //   println("handling raids")
  //   val raidJson: String = fileIO.parseRaidData()
  //   val maybeParsed = parser.decode[List[Raid]](raidJson)
  //   val raidData: List[Raid] = maybeParsed.getOrElse(Nil)
  //
  //   discordBot.sendRaids(raidData)
  // }
}
