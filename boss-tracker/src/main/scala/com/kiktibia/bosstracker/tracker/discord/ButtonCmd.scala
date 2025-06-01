package com.kiktibia.bosstracker.tracker.discord

import net.dv8tion.jda.api.events.interaction.component.ButtonInteractionEvent

trait ButtonCmd extends JDAInteraction {
  def execute(event: ButtonInteractionEvent): Unit
}
