package com.kiktibia.bosstracker.tracker.service

import cats.Applicative
import cats.Monad
import cats.effect.Sync
import cats.implicits.*
import cats.syntax.all.*
import com.kiktibia.bosstracker.tracker.Model.*
import com.kiktibia.bosstracker.tracker.service.FileIO
import io.circe.Error
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.LocalDate
import java.time.OffsetDateTime
import java.time.temporal.ChronoUnit.DAYS
import javax.lang.model.element.TypeElement

class BossPredictor(fileIO: FileIO) {

  def killedBosses(bossStats: List[BossStats]): List[KilledBoss] = {
    bossStats.filter(_.stats.head.killed > 0).map { s =>
      KilledBoss(s.boss, s.stats.head.killed)
    }
  }

  def predictions(bossStats: List[BossStats], date: LocalDate): List[BossChances] = {
    bossStats.filterNot(_.boss.predict.contains(false)).map(b => BossChances(b.boss, getChances(b, date)))
  }

  private def getChances(bossStats: BossStats, date: LocalDate): List[BossChance] = {
    val min = bossStats.boss.windowMin
    val max = bossStats.boss.windowMax
    if (bossStats.boss.spawnPoints == 1) {
      bossStats.stats.find(_.killed > 0) match {
        case Some(lastKilled) =>
          List(getChance(date, lastKilled.date, min, max))
        case None =>
          List(BossChance(Chance.None, 0, min, Some(max))) // Not ideal, but should never happen on old servers
      }
    } else {
      val daysKilled = bossStats.stats.flatMap { dayStats =>
        if (dayStats.killed == 0) Nil
        else if (dayStats.killed > 1)
          List.fill(dayStats.killed)(dayStats.date)
        else List(dayStats.date)
      }
      daysKilled.take(bossStats.boss.spawnPoints).map { killedDate =>
        getChance(date, killedDate, min, max)
      }
    }
  }

  private def getChance(date: LocalDate, lastKilled: LocalDate, min: Int, max: Int): BossChance = {
    val daysSinceKilled = DAYS.between(lastKilled, date).toInt
    val nStartWindowsHigh = daysSinceKilled / min
    val nEndWindowsHigh = (daysSinceKilled - 1) / max + 1
    val nStartWindowsLow = daysSinceKilled / (min - 1)
    val nEndWindowsLow = (daysSinceKilled - 1) / (max + 1) + 1
    val isHighChance = nStartWindowsHigh >= nEndWindowsHigh
    val isLowChance = nStartWindowsLow >= nEndWindowsLow

    val chance =
      if (isHighChance) Chance.High
      else if (isLowChance) Chance.Low
      else Chance.None

    val startOfEndless = (max - 1) / (max - min) * min
    println(startOfEndless)

    val windowMin = math.min(math.max(nStartWindowsHigh, 1) * min, startOfEndless)
    val windowMax = math.max(nStartWindowsHigh, 1) * max
    val windowMaxOpt = if (windowMin >= startOfEndless) None else Some(windowMax)

    BossChance(chance, daysSinceKilled, windowMin, windowMaxOpt)
  }

}
