package com.kiktibia.bosstracker.tracker.service.obs

import cats.effect.IO
import cats.implicits.*
import com.kiktibia.bosstracker.tracker.discord.DiscordBot
import com.kiktibia.bosstracker.tracker.service.FileIO
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*

import java.time.format.DateTimeFormatter

class MwcService(
    fileIO: FileIO,
    discordBot: DiscordBot
) {

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
}
