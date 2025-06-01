package com.kiktibia.bosstracker.tracker.discord

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.*
import com.kiktibia.bosstracker.tracker.repo.BossTrackerRepo
import net.dv8tion.jda.api.entities.Guild
import net.dv8tion.jda.api.entities.Member
import net.dv8tion.jda.api.entities.Role
import net.dv8tion.jda.api.events.interaction.command.SlashCommandInteractionEvent
import net.dv8tion.jda.api.interactions.commands.build.Commands

import java.awt.Color
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.*
import scala.jdk.FutureConverters.*

class CreateRolesCommand(repo: BossTrackerRepo) extends SlashCmd {

  override val name = "create-roles"
  override val slashCommandData = Commands.slash(name, "Create raid alert roles")

  override def execute(event: SlashCommandInteractionEvent): Unit = {
    event.reply("Creating roles").queue()
    val guild = event.getGuild()
    val rolePrefixes = repo.getRolePrefixes().unsafeRunSync()
    val roles = rolePrefixes.flatMap(rp => List(s"$rp-area", s"$rp-subarea", s"$rp-raid"))
    val existingRoles = guild.getRoles().asScala.toList.map(_.getName)
    val rolesToAdd = roles diff existingRoles

    rolesToAdd.foreach { role =>
      val color =
        if (role.endsWith("-area")) "#992d22"
        else if (role.endsWith("-subarea")) "#a84300"
        else "#e67e22"
      guild.createRole().setName(role).setColor(Color.decode(color)).queue()
    }
  }
}
