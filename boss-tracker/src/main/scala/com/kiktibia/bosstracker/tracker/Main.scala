package com.kiktibia.bosstracker.tracker

import cats.effect.*
import cats.syntax.all.*
import com.kiktibia.bosstracker.config.AppConfig
import com.kiktibia.bosstracker.config.Config
import com.kiktibia.bosstracker.tracker.discord.*
import com.kiktibia.bosstracker.tracker.repo.BossTrackerRepo
import com.kiktibia.bosstracker.tracker.service.*
import com.kiktibia.bosstracker.tracker.service.obs.*
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
    val discordBot = new DiscordBot(cfg, repo)
    val bossTrackerService = new BossTrackerService(cfg, fileIO, fetcher, predictor, discordBot)
    val mwcService = new MwcService(fileIO, discordBot)
    val raidService = new RaidService(fileIO, discordBot, repo)

    val mainStream = Stream
      .fixedRateStartImmediately[IO](30.seconds)
      .evalTap { _ =>
        val today = LocalDate.now()
        val now = ZonedDateTime.now()
        (for
          _ <- fileIO.updateBossStatsRepo()
          _ <- bossTrackerService.handleKilledBossUpdate(today)
          _ <- bossTrackerService.handlePredictionsUpdate(today, now)
          _ <- mwcService.checkForMwcUpdate()
        yield ())
          .handleErrorWith { e =>
            Logger[IO].warn(e)(s"Recovering from error in main stream:${System.lineSeparator}")
          }
      }.void

    val raidStream = Stream
      .fixedRateStartImmediately[IO](5.seconds)
      .evalTap { _ =>
        raidService.checkForRaidUpdates()
          .handleErrorWith { e =>
            Logger[IO].warn(e)(s"Recovering from error in raid stream:${System.lineSeparator}")
          }
      }.void

    mainStream.merge(raidStream).compile.drain.map(_ => ExitCode.Success)
  }
}
