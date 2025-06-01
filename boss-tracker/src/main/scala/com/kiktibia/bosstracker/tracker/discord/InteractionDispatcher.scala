package com.kiktibia.bosstracker.tracker.discord

import net.dv8tion.jda.api.events.interaction.command.SlashCommandInteractionEvent
import net.dv8tion.jda.api.events.interaction.component.ButtonInteractionEvent
import net.dv8tion.jda.api.hooks.ListenerAdapter

class InteractionDispatcher(
  slashInteractions: List[SlashCmd],
  buttonInteractions: List[ButtonCmd]
) extends ListenerAdapter {

  override def onSlashCommandInteraction(e: SlashCommandInteractionEvent): Unit =
    slashInteractions.find(_.name == e.getName).foreach(_.execute(e))

  override def onButtonInteraction(e: ButtonInteractionEvent): Unit =
    buttonInteractions.find(i => e.getComponentId.startsWith(i.name)).foreach(_.execute(e))
}
