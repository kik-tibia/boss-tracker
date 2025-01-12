package com.kiktibia.bosstracker.tracker

import cats.effect.*
import cats.syntax.all.*
import com.kiktibia.bosstracker.config.AppConfig
import com.kiktibia.bosstracker.config.Config
import com.kiktibia.bosstracker.tracker.service.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.LocalDate
import java.time.ZonedDateTime
import scala.concurrent.duration.*

object Main extends IOApp {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  override protected def blockedThreadDetectionEnabled = true

  override def run(args: List[String]): IO[ExitCode] = {
    for
      cfg <- AppConfig.config.load[IO]
      run <- runWithConfig(cfg)
    yield run
  }

  private def runWithConfig(cfg: Config) = {
    val fileIO = new FileIO(cfg)
    val fetcher = new BossDataFetcher(fileIO)
    val predictor = new BossPredictor(fileIO)
    val discordBot = new DiscordBot(cfg)
    val bossTrackerService = new BossTrackerService(cfg, fileIO, fetcher, predictor, discordBot)
    val obsService = new ObsService(cfg, fileIO, discordBot)
    println("cfg:")
    println(cfg.bot.mwcTimingChannelNames)

    Stream
      .fixedRateStartImmediately[IO](30.seconds)
      .evalTap { _ =>
        IO {
          println("Running stream")
          val today = LocalDate.now()
          val now = ZonedDateTime.now()
          fileIO.updateBossStatsRepo()
          bossTrackerService.handleKilledBossUpdate(today)
          bossTrackerService.handlePredictionsUpdate(today, now)
          obsService.handleMwcUpdate()
          // obsService.handleRaidsUpdate()
        }
      }
      .compile
      .drain
      .map(_ => ExitCode.Success)
  }

}
