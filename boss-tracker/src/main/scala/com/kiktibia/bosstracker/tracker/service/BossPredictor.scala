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

  def predictions(bossStats: List[BossStats], date: LocalDate): List[(Boss, List[Chance])] = {
    bossStats.filterNot(_.boss.predict.contains(false)).map(b => (b.boss, getChances(b, date)))
  }

  private def getChances(bossStats: BossStats, date: LocalDate): List[Chance] = {
    val min = bossStats.boss.windowMin
    val max = bossStats.boss.windowMax
    if (bossStats.boss.spawnPoints == 1) {
      bossStats.stats.find(_.killed > 0) match {
        case Some(lastKilled) =>
          List(getChance(date, lastKilled.date, min, max))
        case None => List(Chance.None)
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

  private def getChance(date: LocalDate, lastKilled: LocalDate, min: Int, max: Int): Chance = {
    val daysSinceKilled = DAYS.between(lastKilled, date)
    val nStartWindowsHigh = daysSinceKilled / min
    val nEndWindowsHigh = (daysSinceKilled - 1) / max + 1
    val nStartWindowsLow = daysSinceKilled / (min - 1)
    val nEndWindowsLow = (daysSinceKilled - 1) / (max + 1) + 1
    val isHighChance = nStartWindowsHigh >= nEndWindowsHigh
    val isLowChance = nStartWindowsLow >= nEndWindowsLow
    if (isHighChance) Chance.High
    else if (isLowChance) Chance.Low
    else Chance.None
  }

}
