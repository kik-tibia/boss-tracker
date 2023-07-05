package com.kiktibia.bosstracker.tracker.service

import com.kiktibia.bosstracker.tracker.service.FileIO
import com.kiktibia.bosstracker.config.Config
import java.time.LocalDate
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.ZoneId
import java.time.LocalTime

class BossTrackerService(
    cfg: Config,
    fileIO: FileIO,
    fetcher: BossDataFetcher,
    predictor: BossPredictor,
    discordBot: DiscordBot
) {

  def handleKilledBossUpdate(today: LocalDate): Unit = {
    if (shouldPostKilledUpdate()) {
      println("Posting killed bosses update")
      for
        bd <- fetcher.getAllBossData()
        killed <- predictor.killedBosses(bd)
        _ = discordBot.sendKilled(killed, today)
      yield ()
      fileIO.updateDateFile(cfg.file.statsDateFileName, today)
    }
  }

  def handlePredictionsUpdate(today: LocalDate, now: ZonedDateTime): Unit = {
    if (shouldPostPredictionsUpdate(today, now)) {
      println("Posting predictions")
      for
        bd <- fetcher.getAllBossData()
        chances <- predictor.predictions(bd, today)
        _ = discordBot.sendPredictions(chances, today)
      yield ()
      fileIO.updateDateFile(cfg.file.predictionsDateFileName, today)
    }
  }

  private def shouldPostKilledUpdate(): Boolean = {
    val lastUpdate = fileIO.getLastKilledUpdate()
    val latestStatsDate = fileIO
      .parseLatestStats()
      .map(s => LocalDate.parse(s._2.timestamp, DateTimeFormatter.ISO_DATE_TIME))

    latestStatsDate.exists(_.isAfter(lastUpdate))
  }

  private def shouldPostPredictionsUpdate(date: LocalDate, currentTime: ZonedDateTime): Boolean = {
    val lastUpdate = fileIO.getLastPredictionsUpdate()
    val serverSaveTime = ZonedDateTime.of(date, LocalTime.of(10, 0), ZoneId.of("Europe/Berlin"))
    date != lastUpdate && currentTime.isAfter(serverSaveTime)
  }
}
