package com.kiktibia.bosstracker.tracker.discord

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.*
import net.dv8tion.jda.api.entities.Guild
import net.dv8tion.jda.api.entities.Member
import net.dv8tion.jda.api.entities.Role
import net.dv8tion.jda.api.entities.emoji.Emoji
import net.dv8tion.jda.api.events.interaction.GenericInteractionCreateEvent
import net.dv8tion.jda.api.events.interaction.command.SlashCommandInteractionEvent
import net.dv8tion.jda.api.events.interaction.component.ButtonInteractionEvent
import net.dv8tion.jda.api.interactions.commands.build.Commands
import net.dv8tion.jda.api.interactions.components.ActionRow
import net.dv8tion.jda.api.interactions.components.buttons.Button

import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.*
import scala.jdk.FutureConverters.*

class ChooseRolesCommand extends SlashCmd with ButtonCmd {

  override val name = "choose-roles"
  override val slashCommandData = Commands.slash(name, "Choose your roles")

  override def execute(event: SlashCommandInteractionEvent): Unit = {
    event.reply("Choose your roles")
      .addComponents(makeRows(0, event))
      .queue()
  }

  override def execute(event: ButtonInteractionEvent): Unit = {
    if (event.getComponentId.startsWith(s"$name:role_toggle:"))
      roleToggleButtonClick(event)
    else if (event.getComponentId.startsWith(s"$name:nav:"))
      navButtonClick(event)
  }

  private def roleToggleButtonClick(event: ButtonInteractionEvent): Unit = {
    val roleId = event.getComponentId.stripPrefix(s"$name:role_toggle:").toLong

    val roleUpdate = Option(event.getGuild.getRoleById(roleId)).map { role =>
      if (event.getMember.getRoles.contains(role)) {
        event.getGuild.removeRoleFromMember(event.getMember, role)
      } else {
        event.getGuild.addRoleToMember(event.getMember, role)
      }
    }.toList.map(restActionIO)

    val currentPage = event.getMessage.getActionRows.get(event.getMessage.getActionRows.size() - 1)
      .getButtons.asScala.collect {
        case b if b.getId.startsWith(s"$name:nav:prev:") =>
          b.getId.split(":")(3).toInt
      }.headOption.getOrElse(0)

    (for
      _ <- restActionIO(event.deferEdit())
      _ <- roleUpdate.parSequence
      m <- restActionIO(event.getGuild.retrieveMemberById(event.getMember.getId))
      _ <- rerenderIO(event, currentPage)
    yield ()).unsafeRunAndForget()
  }

  private def navButtonClick(event: ButtonInteractionEvent): Unit = {
    val parts = event.getComponentId.split(":")
    val dir = parts(2)
    val currentPage = parts(3).toInt
    val nextPage = dir match {
      case "prev" => (currentPage - 1) max 0
      case "next" => (currentPage + 1) min (getRolePages(event).size - 1)
    }
    event.deferEdit().queue(_ =>
      event.getHook.editOriginalComponents(makeRows(nextPage, event)).queue()
    )
  }

  private def rerenderIO(event: ButtonInteractionEvent, currentPage: Int): IO[Unit] =
    for {
      rows <- IO.delay(makeRows(currentPage, event))
      _ <- restActionIO(event.getHook.editOriginalComponents(rows))
    } yield ()

  private def getRolePages(event: GenericInteractionCreateEvent): List[List[Role]] = {
    val roles: List[Role] = event.getGuild.getRoles.asScala.toList.filter { r =>
      r.getName.endsWith("-area") || r.getName.endsWith("-subarea") || r.getName.endsWith("-raid")
    }
    roles.sortBy(_.getName).grouped(3).map(_.sortBy(_.getName.reverse))
      .flatten.toList.grouped(12).toList
  }

  private def makeRows(pageIndex: Int, event: GenericInteractionCreateEvent): java.util.List[ActionRow] = {
    val currentPages = getRolePages(event)
    // Role buttons, 3 per row
    val roleRows = currentPages(pageIndex)
      .grouped(3)
      .map { chunk =>
        val buttons = chunk.map { role =>
          val roleId = role.getIdLong
          val hasRole = Option(role).exists(r => event.getMember.getRoles.contains(r))
          val id = s"$name:role_toggle:$roleId"
          val label = Option(role).map(_.getName).getOrElse("unknown")
          if (hasRole)
            Button.success(id, label)
          else
            Button.secondary(id, label)
        }
        ActionRow.of(buttons.asJava)
      }

    // Navigation row
    val prevButton = Button.secondary(
      s"$name:nav:prev:$pageIndex",
      Emoji.fromUnicode("⬅️")
    )

    val pageButton = Button.secondary(
      s"$name:nav:page",
      s"Page ${pageIndex + 1} of ${currentPages.length}"
    ).asDisabled

    val nextButton = Button.secondary(
      s"$name:nav:next:$pageIndex",
      Emoji.fromUnicode("➡️")
    )

    val prev = if (pageIndex == 0) prevButton.asDisabled() else prevButton
    val next = if (pageIndex == currentPages.size - 1) nextButton.asDisabled else nextButton

    (roleRows.toList :+ ActionRow.of(prev, pageButton, next)).asJava
  }

}
