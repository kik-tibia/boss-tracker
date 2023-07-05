package com.kiktibia.bosstracker.tracker

import java.net.URLEncoder
import java.time.LocalDate

object Model {

  enum Chance:
    case None, Low, High

  case class DayStats(date: LocalDate, killed: Int)

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
    def emojiName: String =
      s"$categoryToEmoji $name"

    def guildstatsName(world: String): String =
      s"[$name](https://guildstats.eu/bosses?world=${world.capitalize}&monsterName=${URLEncoder.encode(raceName.getOrElse(name))})"

    def emojiCategory: String = s"$categoryToEmoji $category $categoryToEmoji"

    private def categoryToEmoji: String = category match {
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

  case class Information(
      api_version: Int,
      timestamp: String
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

  case class KillStatsDay(
      killstatistics: KillStatistics,
      information: Information
  )

}
