package com.kiktibia.bosstracker.tracker

import cats.effect.*
import cats.syntax.all.*
import com.kiktibia.bosstracker.config.AppConfig
import com.kiktibia.bosstracker.config.Config
import com.kiktibia.bosstracker.tracker.repo.BossTrackerRepo
import com.kiktibia.bosstracker.tracker.discord.*
import com.kiktibia.bosstracker.tracker.service.*
import doobie.util.transactor.Transactor
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
    val tx: Transactor[IO] = Transactor.fromDriverManager[IO](
      driver = "org.postgresql.Driver",
      url = cfg.db.url,
      user = cfg.db.user,
      password = cfg.db.password,
      logHandler = None
    )
    val repo = new BossTrackerRepo(tx)
    val fileIO = new FileIO(cfg)
    val fetcher = new BossDataFetcher(fileIO)
    val predictor = new BossPredictor(fileIO)
    val discordBot = new DiscordBot(cfg)
    val bossTrackerService = new BossTrackerService(cfg, fileIO, fetcher, predictor, discordBot)
    val obsService = new ObsService(fileIO, discordBot, repo)

    Stream
      .fixedRateStartImmediately[IO](30.seconds)
      .evalTap { _ =>
        println("Running stream")
        val today = LocalDate.now()
        val now = ZonedDateTime.now()
        for
          _ <- fileIO.updateBossStatsRepo()
          _ <- bossTrackerService.handleKilledBossUpdate(today)
          _ <- bossTrackerService.handlePredictionsUpdate(today, now)
          _ <- obsService.checkForMwcUpdate()
          _ <- obsService.checkForRaidUpdates()
        yield ()
      }
      .compile
      .drain
      .map(_ => ExitCode.Success)
  }

}
