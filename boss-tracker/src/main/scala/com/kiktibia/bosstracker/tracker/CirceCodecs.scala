package com.kiktibia.bosstracker.tracker

import java.time.format.DateTimeFormatter
import io.circe.*
import io.circe.parser.*
import io.circe.generic.auto.*
import cats.syntax.all.*

import java.time.LocalDate

trait CirceCodecs {
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  given localDateEncoder: Encoder[LocalDate] = Encoder.encodeString.contramap[LocalDate](_.format(formatter))

  given localDateDecoder: Decoder[LocalDate] = Decoder.decodeString.emap[LocalDate](str => {
    Either.catchNonFatal(LocalDate.parse(str, formatter)).leftMap(_.getMessage)
  })

}
