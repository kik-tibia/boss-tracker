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
    statsRepoPath: String
)

final case class BotConfig(
    token: String,
    guildId: String,
    killedChannelName: String,
    predictionsChannelName: String
)

final case class Config(
    general: GeneralConfig,
    file: FileConfig,
    bot: BotConfig
)

object AppConfig {
  private val generalConfig: ConfigValue[Effect, GeneralConfig] = (
    env("WORLD").as[String]
  ).map(GeneralConfig.apply)

  private val fileConfig: ConfigValue[Effect, FileConfig] = (
    env("STATS_DATE_FILE").as[String],
    env("PREDICTIONS_DATE_FILE").as[String],
    env("BOSS_LIST_FILE").as[String],
    env("DATE_INFO_FILE").as[String],
    env("STATS_REPO_PATH").as[String]
  ).parMapN(FileConfig.apply)

  private val botConfig: ConfigValue[Effect, BotConfig] = (
    env("TOKEN").as[String],
    env("GUILD_ID").as[String],
    env("KILLED_CHANNEL_NAME").as[String],
    env("PREDICTIONS_CHANNEL_NAME").as[String]
  ).parMapN(BotConfig.apply)

  val config: ConfigValue[Effect, Config] = (generalConfig, fileConfig, botConfig).parMapN(Config.apply)
}
