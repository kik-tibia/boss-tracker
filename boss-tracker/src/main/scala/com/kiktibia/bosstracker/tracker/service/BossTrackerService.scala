package com.kiktibia.bosstracker.tracker.service

import cats.data.EitherT
import cats.effect.IO
import com.kiktibia.bosstracker.config.Config
import com.kiktibia.bosstracker.tracker.ObsModel.Raid
import com.kiktibia.bosstracker.tracker.discord.DiscordBot
import com.kiktibia.bosstracker.tracker.service.FileIO

import java.time.LocalDate
import java.time.LocalTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

class BossTrackerService(
    cfg: Config,
    fileIO: FileIO,
    fetcher: BossDataFetcher,
    predictor: BossPredictor,
    discordBot: DiscordBot
) {

  def handleKilledBossUpdate(today: LocalDate): IO[Unit] =
    shouldPostKilledUpdate()
      .ifM(
        (for
          bd <- fetcher.getAllBossData()
          killed = predictor.killedBosses(bd)
          _ = discordBot.sendKilled(killed, today)
          _ <- EitherT.liftF(fileIO.updateDateFile(cfg.file.statsDateFileName, today))
        yield ()).value,
        IO.pure(Right(()))
      )
      .map(_ => ())

  def handlePredictionsUpdate(today: LocalDate, now: ZonedDateTime): IO[Unit] =
    shouldPostPredictionsUpdate(today, now)
      .ifM(
        (for
          bd <- fetcher.getAllBossData()
          chances = predictor.predictions(bd, today)
          _ = discordBot.sendPredictions(chances, today)
          _ <- EitherT.liftF(fileIO.updateDateFile(cfg.file.predictionsDateFileName, today))
        yield ()).value,
        IO.pure(Right(()))
      )
      .map(_ => ())

  private def shouldPostKilledUpdate(): IO[Boolean] = {
    (for
      lastUpdate <- EitherT.liftF(fileIO.getLastKilledUpdate())
      latestStatsDate <- fileIO
        .parseLatestStats()
        .map(s => LocalDate.parse(s._2.timestamp, DateTimeFormatter.ISO_DATE_TIME))
    yield latestStatsDate.isAfter(lastUpdate)).valueOr(_ => false)
  }

  private def shouldPostPredictionsUpdate(date: LocalDate, currentTime: ZonedDateTime): IO[Boolean] = {
    for
      lastUpdate <- fileIO.getLastPredictionsUpdate()
      serverSaveTime = ZonedDateTime.of(date, LocalTime.of(10, 0), ZoneId.of("Europe/Berlin"))
    yield date != lastUpdate && currentTime.isAfter(serverSaveTime)
  }
}
