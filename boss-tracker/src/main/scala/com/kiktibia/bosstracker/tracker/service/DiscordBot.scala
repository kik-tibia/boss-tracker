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
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import javax.lang.model.element.TypeElement
import scala.jdk.CollectionConverters.*

class DiscordBot(cfg: Config) {

  private val jda = JDABuilder.createDefault(cfg.bot.token).build()
  jda.awaitReady()
  private val guild: Guild = jda.getGuildById(cfg.bot.guildId)

  def sendKilled(bosses: List[KilledBoss], today: LocalDate): Unit = {
    val killedChannel =
      guild.getTextChannels().asScala.toList.find(_.getName() == cfg.bot.killedChannelName).get

    val killedBossesList = bosses.map(_.killedString).mkString("\n")
    killedChannel.sendMessage(s"**Bosses killed yesterday (${today.minusDays(1)}):**\n$killedBossesList").queue()
  }

  def sendPredictions(chances: List[BossChances], today: LocalDate): Unit = {
    val predictionsHighChannel =
      guild.getTextChannels().asScala.toList.find(_.getName() == cfg.bot.predictionsHighChannelName).get
    val predictionsAllChannel =
      guild.getTextChannels().asScala.toList.find(_.getName() == cfg.bot.predictionsAllChannelName).get

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

    predictionsAllChannel.sendMessage(s"**All boss predictions for $today**").queue()
    predictions.map(_._2).foreach { p =>
      predictionsAllChannel.sendMessageEmbeds(categoryPredictionsEmbed(p, highOnly = false)).queue()
    }
    predictionsHighChannel.sendMessage(s"**High chance boss predictions for $today**").queue()
    predictions.map(_._2).foreach { p =>
      predictionsHighChannel.sendMessageEmbeds(categoryPredictionsEmbed(p, highOnly = true)).queue()
    }

  }

  def sendMwc(msg: String): Unit = {
    val mwcChannel =
      guild.getTextChannels().asScala.toList.find(_.getName() == cfg.bot.mwcChannelName).get
    mwcChannel.sendMessage(msg).queue()
  }

  def sendRaids(raids: List[Raid]): Unit = {
    val raidChannel =
      guild.getTextChannels().asScala.toList.find(_.getName() == cfg.bot.raidChannelName).get
    println(raids)
    raids.foreach { raid =>
      raidChannel.sendMessageEmbeds(raidEmbed(raid)).queue()
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

  private def raidEmbed(raid: Raid): MessageEmbed = {
    new EmbedBuilder()
      .setTitle(raid.category)
      .setDescription(
        s"area: ${raid.areaName} - ${raid.subareaName}\nraid: ${raid.raidTypeId}\nstart date: ${raid.startDate.toString()}"
      )
      .build()
  }
}
