package com.kiktibia.bosstracker.tracker

import java.net.URLEncoder
import java.time.LocalDate

object Model {

  enum Chance:
    case None, Low, High

  case class DayStats(date: LocalDate, killed: Int, playersKilled: Int)

  case class BossStats(boss: Boss, stats: List[DayStats])

  case class Boss(
      name: String,
      raceName: Option[String],
      predict: Option[Boolean],
      windowMin: Int,
      windowMax: Int,
      spawnPoints: Int,
      category: String
  ) {
    def guildstatsName(world: String): String =
      s"[$name](https://guildstats.eu/bosses?world=${world.capitalize}&monsterName=${URLEncoder.encode(raceName.getOrElse(name))})"

    def emojiCategory: String = s"$categoryToEmoji $category $categoryToEmoji"

    def categoryToEmoji: String = category match {
      case "World Bosses" => ":earth_africa:"
      case "Profitable" => ":moneybag:"
      case "POI" => ":fire:"
      case "Vampire Lords" => ":vampire:"
      case "Zao" => ":lizard:"
      case "Stealth Ring" => ":ring:"
      case "Bank Robber" => ":bank:"
      case "Other" => ":mage:"
      case "Hive Outpost" => ":bug:"
      case "Varying Spawn" => ":repeat:"
      case "Rookgaard" => ":hatching_chick:"
      case _ => ""
    }
  }

  case class BossChance(chance: Chance, daysSince: Int, windowMin: Int, windowMax: Option[Int]) {
    def toPredictionString(): String = {
      val chanceEmoji = chance match {
        case Chance.None => ":red_circle:"
        case Chance.Low => ":yellow_circle:"
        case Chance.High => ":green_circle:"
      }
      val window = windowMax match {
        case Some(m) => s"($windowMin–$m)"
        case None => s"($windowMin+)"
      }
      s" — $chanceEmoji $daysSince $window"
    }
  }

  case class BossChances(boss: Boss, chances: List[BossChance])

  case class KilledBoss(boss: Boss, numberKilled: Int) {
    def killedString: String = {
      val nKilledString =
        if (numberKilled > 1) s"(x$numberKilled)"
        else ""
      s"${boss.categoryToEmoji} ${boss.name} $nKilledString"
    }
  }

  case class BossList(bosses: List[Boss])

  case class DateInfo(startDate: LocalDate, missingDates: List[LocalDate])

  case class KillHistory(date: LocalDate, kills: Int)

  case class BossKillHistory(killHistory: List[KillHistory])

  case class Entry(
      race: String,
      last_day_players_killed: Int,
      last_day_killed: Int,
      last_week_players_killed: Int,
      last_week_killed: Int
  )

  case class Total(
      last_day_players_killed: Int,
      last_day_killed: Int,
      last_week_players_killed: Int,
      last_week_killed: Int
  )

  case class KillStatistics(
      world: String,
      entries: Seq[Entry],
      total: Total
  )

  case class Api(version: Int, release: String, commit: String)
  case class Status(http_code: Int)

  case class Information(
      api_version: Option[Int],
      api: Option[Api],
      status: Option[Status],
      timestamp: String
  )

  case class KillStatsDay(
      killstatistics: KillStatistics,
      information: Information
  )

}
