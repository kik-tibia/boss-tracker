package com.kiktibia.bosstracker.tracker.service

import cats.Applicative
import cats.Monad
import cats.effect.Sync
import cats.implicits.*
import cats.syntax.all.*
import com.kiktibia.bosstracker.config.Config
import com.kiktibia.bosstracker.tracker.Model.*
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

  def sendKilled(bosses: List[Boss], today: LocalDate): Unit = {
    val killedChannel =
      guild.getTextChannels().asScala.toList.find(_.getName() == cfg.bot.killedChannelName).get

    val killedBossesList = bosses.map(_.emojiName).mkString("\n")
    killedChannel.sendMessage(s"**Bosses killed yesterday (${today.minusDays(1)}):**\n$killedBossesList").queue()
  }

  def sendPredictions(chances: List[(Boss, List[Chance])], today: LocalDate): Unit = {
    val predictionsChannel =
      guild.getTextChannels().asScala.toList.find(_.getName() == cfg.bot.predictionsChannelName).get

    val order = List(
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

    predictionsChannel.sendMessage(s"**Boss predictions for $today**").queue()
    predictions.map(_._2).foreach { p =>
      predictionsChannel.sendMessageEmbeds(categoryPredictionsEmbed(p)).queue()
    }

  }

  private def categoryPredictionsEmbed(chances: List[(Boss, List[Chance])]): MessageEmbed = {
    val categoryString = chances.head._1.emojiCategory
    val bossList = chances
      .filter(_._2.exists(c => c == Chance.Low || c == Chance.High))
      .map { case (boss, chances) =>
        val chanceEmojis = chances
          .map {
            case Chance.None => ":red_circle:"
            case Chance.Low => ":yellow_circle:"
            case Chance.High => ":green_circle:"
          }
          .mkString(" ")
        s"$chanceEmojis ${boss.guildstatsName(cfg.general.world)}"
      }
      .mkString("\n")
    new EmbedBuilder()
      .setTitle(s"**$categoryString**")
      .setDescription(bossList)
      .setColor(6386874)
      .build()
  }
}
