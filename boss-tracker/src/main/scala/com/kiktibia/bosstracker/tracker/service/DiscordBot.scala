package com.kiktibia.bosstracker.tracker.service

import cats.Applicative
import cats.Monad
import cats.effect.Sync
import cats.implicits.*
import cats.syntax.all.*
import com.kiktibia.bosstracker.config.Config
import com.kiktibia.bosstracker.tracker.Model.*
import com.kiktibia.bosstracker.tracker.ObsModel.Raid
import io.circe.Error
import net.dv8tion.jda.api.EmbedBuilder
import net.dv8tion.jda.api.JDABuilder
import net.dv8tion.jda.api.entities.Guild
import net.dv8tion.jda.api.entities.MessageEmbed
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters.*
import com.kiktibia.bosstracker.tracker.repo.RaidDto

class DiscordBot(cfg: Config) {

  private val jda = JDABuilder.createDefault(cfg.bot.token).build()
  jda.awaitReady()
  private val guilds: List[Guild] = jda.getGuilds().asScala.toList
  println(guilds.toString())

  def sendKilled(bosses: List[KilledBoss], today: LocalDate): Unit = {
    val killedBossesList = bosses.map(_.killedString).mkString("\n")
    val message = s"**Bosses killed yesterday (${today.minusDays(1)}):**\n$killedBossesList"
    sendMessageToChannel(message, cfg.bot.killedChannelNames)
  }

  def sendPredictions(chances: List[BossChances], today: LocalDate): Unit = {
    val order = List(
      "World Bosses",
      "Profitable",
      "POI",
      "Vampire Lords",
      "Zao",
      "Stealth Ring",
      "Varying Spawn",
      "Hive Outpost",
      "Other",
      "Rookgaard"
    )
    val predictions = chances.groupBy(_._1.category).toList.sortWith { case ((a, _), (b, _)) =>
      order.indexOf(a) < order.indexOf(b)
    }

    val highPredictionsEmbeds = predictions.map(_._2).map(p => categoryPredictionsEmbed(p, highOnly = true))
    val allPredictionsEmbeds = predictions.map(_._2).map(p => categoryPredictionsEmbed(p, highOnly = false))

    sendMessageToChannel(s"**High chance boss predictions for $today**", cfg.bot.predictionsHighChannelNames)
    sendEmbedsToChannel(highPredictionsEmbeds, cfg.bot.predictionsHighChannelNames)
    sendMessageToChannel(s"**All boss predictions for $today**", cfg.bot.predictionsAllChannelNames)
    sendEmbedsToChannel(allPredictionsEmbeds, cfg.bot.predictionsAllChannelNames)
  }

  def sendMwcTiming(message: String): Unit = {
    sendMessageToChannel(message, cfg.bot.mwcTimingChannelNames)
  }

  def sendMwcDetails(message: String): Unit = {
    sendMessageToChannel(message, cfg.bot.mwcDetailsChannelNames)
  }

  def sendRaidUpdate(obsRaid: Raid, maybeRaidDto: Option[RaidDto]): Unit = {
    sendEmbedsToChannel(List(raidEmbed(obsRaid, maybeRaidDto)), cfg.bot.raidChannelNames)
  }

  private def sendMessageToChannel(message: String, channelNames: List[String]): Unit =
    guilds.foreach { g =>
      g.getTextChannels()
        .asScala
        .find(guildChannel => channelNames.exists(c => guildChannel.getName().endsWith(c)))
        .foreach { channel =>
          channel.sendMessage(message).queue()
        }
    }

  private def sendEmbedsToChannel(embeds: List[MessageEmbed], channelNames: List[String]): Unit =
    guilds.foreach { g =>
      g.getTextChannels()
        .asScala
        .find(guildChannel => channelNames.exists(c => guildChannel.getName().endsWith(c)))
        .foreach { channel =>
          embeds.foreach { embed =>
            channel.sendMessageEmbeds(embed).queue()
          }
        }
    }

  private def categoryPredictionsEmbed(bossChances: List[BossChances], highOnly: Boolean): MessageEmbed = {
    val categoryString = bossChances.head.boss.emojiCategory
    val bossesToPost = bossChances
      .filter(_.chances.exists(c => !highOnly || c.chance == Chance.High))

    val bossList =
      if (bossesToPost.isEmpty) "No high chance bosses in this category"
      else
        bossesToPost
          .map { case BossChances(boss, chances) =>
            val filteredChances = chances.filter(c => !highOnly || c.chance == Chance.High)
            s"${boss.guildstatsName(cfg.general.world)}${filteredChances.map(_.toPredictionString()).mkString}"
          }
          .mkString("\n")

    new EmbedBuilder()
      .setTitle(s"**$categoryString**")
      .setDescription(bossList)
      .setColor(6386874)
      .build()
  }

  private def raidEmbed(obsRaid: Raid, maybeRaidDto: Option[RaidDto]): MessageEmbed = {
    val raidType = maybeRaidDto.flatMap(_.raidType)
    val title = raidType.map(_.name).getOrElse("Upcoming Raid")
    val description = raidType.map(_.message).getOrElse("")
    val area = obsRaid.areaName.getOrElse("Not announced")
    val subarea = obsRaid.subareaName.getOrElse("Not announced")
    val startTime = s"<t:${obsRaid.startDate.toEpochSecond()}:T>"

    new EmbedBuilder()
      .setTitle(title)
      .setDescription(description)
      .addField("Start time", startTime, false)
      .addField("Area", area, true)
      .addField("Subarea", subarea, true)
      .build()
  }
}
