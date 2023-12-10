package com.kiktibia.bosstracker.tracker.service

import cats.Applicative
import cats.Monad
import cats.effect.Sync
import cats.implicits.*
import cats.syntax.all.*
import com.kiktibia.bosstracker.config.FileConfig
import com.kiktibia.bosstracker.tracker.service.FileIO
import com.kiktibia.bosstracker.tracker.Model.*
import io.circe.Error
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.LocalDate
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import javax.lang.model.element.TypeElement
import com.kiktibia.bosstracker.config.Config

class BossDataFetcher(fileIO: FileIO) {

  def getAllBossData(): Either[Error, List[BossStats]] = {
    for
      dateInfo <- fileIO.getDateInfo()
      bossList <- fileIO.parseBossFile()
      bossData = getBossData(bossList, dateInfo)
    yield bossData
  }

  private def getBossData(bossList: BossList, dateInfo: DateInfo): List[BossStats] = {
    val killStatsDays: List[KillStatsDay] = fileIO
      .parseAllHistoricStats()
      .sortBy(_.information.timestamp)

    bossList.bosses.map { boss =>
      val bossName: String = boss.raceName.getOrElse(boss.name)
      val seenDays = killStatsDays.filter {
        _.killstatistics.entries.exists { e =>
          e.race == bossName && (e.last_day_killed > 0 || e.last_day_players_killed > 0)
        }
      }
      val stats = seenDays
        .map { sd =>
          DayStats(
            LocalDate.parse(sd.information.timestamp, DateTimeFormatter.ISO_DATE_TIME).minusDays(1),
            sd.killstatistics.entries.find(_.race == bossName).map(_.last_day_killed).getOrElse(0),
            sd.killstatistics.entries.find(_.race == bossName).map(_.last_day_players_killed).getOrElse(0)
          )
        }
      BossStats(boss, fillBlankDays(dateInfo.startDate, LocalDate.now().minusDays(1), stats))
    }
  }

  private def fillBlankDays(startDate: LocalDate, endDate: LocalDate, stats: List[DayStats]): List[DayStats] = {
    val start = startDate.toEpochDay
    val end = endDate.toEpochDay
    val dates = (start to end).reverse.map(LocalDate.ofEpochDay(_)).toList
    dates.map { case date =>
      stats.find(_.date == date) match {
        case Some(stat) => stat
        case None => DayStats(date, 0, 0)
      }
    }
  }
}
