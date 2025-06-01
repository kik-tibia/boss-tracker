package com.kiktibia.bosstracker.tracker.discord

import net.dv8tion.jda.api.events.interaction.command.SlashCommandInteractionEvent
import net.dv8tion.jda.api.interactions.commands.build.SlashCommandData

trait SlashCmd extends JDAInteraction {
  val slashCommandData: SlashCommandData
  def execute(event: SlashCommandInteractionEvent): Unit
}
