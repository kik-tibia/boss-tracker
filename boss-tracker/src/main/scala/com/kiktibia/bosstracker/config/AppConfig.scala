package com.kiktibia.bosstracker.config

import cats.effect.implicits.*
import cats.syntax.all.*
import ciris.*

final case class GeneralConfig(
    world: String
)

final case class FileConfig(
    statsDateFileName: String,
    predictionsDateFileName: String,
    bossListFileName: String,
    dateInfoFileName: String,
    statsRepoPath: String,
    missingDataPath: String,
    lastMwcPostFileName: String,
    mwcHistoryPath: String,
    raidDataFile: String
)

final case class BotConfig(
    token: String,
    killedChannelNames: List[String],
    predictionsHighChannelNames: List[String],
    predictionsAllChannelNames: List[String],
    mwcTimingChannelNames: List[String],
    mwcDetailsChannelNames: List[String],
    raidChannelNames: List[String]
)

final case class Config(
    general: GeneralConfig,
    file: FileConfig,
    bot: BotConfig
)

object AppConfig {
  implicit private val listConfigDecoder: ConfigDecoder[String, List[String]] =
    ConfigDecoder[String, String].map(_.split(",").map(_.trim).filter(_.nonEmpty).toList)

  private val generalConfig: ConfigValue[Effect, GeneralConfig] = (
    env("WORLD").as[String]
  ).map(GeneralConfig.apply)

  private val fileConfig: ConfigValue[Effect, FileConfig] = (
    env("STATS_DATE_FILE").as[String],
    env("PREDICTIONS_DATE_FILE").as[String],
    env("BOSS_LIST_FILE").as[String],
    env("DATE_INFO_FILE").as[String],
    env("STATS_REPO_PATH").as[String],
    env("MISSING_DATA_PATH").as[String],
    env("LAST_MWC_POST_FILE").as[String],
    env("MWC_HISTORY_PATH").as[String],
    env("RAID_DATA_FILE").as[String],
  ).parMapN(FileConfig.apply)

  private val botConfig: ConfigValue[Effect, BotConfig] = (
    env("TOKEN").as[String],
    env("KILLED_CHANNEL_NAMES").as[List[String]],
    env("PREDICTIONS_HIGH_CHANNEL_NAMES").as[List[String]],
    env("PREDICTIONS_ALL_CHANNEL_NAMES").as[List[String]],
    env("MWC_TIMING_CHANNEL_NAMES").as[List[String]],
    env("MWC_DETAILS_CHANNEL_NAMES").as[List[String]],
    env("RAID_CHANNEL_NAMES").as[List[String]]
  ).parMapN(BotConfig.apply)

  val config: ConfigValue[Effect, Config] = (generalConfig, fileConfig, botConfig).parMapN(Config.apply)
}
