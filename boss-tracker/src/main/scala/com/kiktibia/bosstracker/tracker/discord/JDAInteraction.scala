package com.kiktibia.bosstracker.tracker.discord

import cats.effect.IO
import net.dv8tion.jda.api.requests.RestAction

import scala.jdk.CollectionConverters.*
import scala.jdk.FutureConverters.*

trait JDAInteraction {
  val name: String

  def restActionIO[A](ra: RestAction[A]): IO[A] =
    IO.fromFuture(IO(ra.submit().asScala))
}
