package com.kiktibia.bosstracker.tracker

import java.time.format.DateTimeFormatter
import io.circe.*
import io.circe.parser.*
import io.circe.generic.auto.*
import cats.syntax.all.*

import java.time.LocalDate
import java.time.ZonedDateTime

trait CirceCodecs {
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  given localDateEncoder: Encoder[LocalDate] = Encoder.encodeString.contramap[LocalDate](_.format(formatter))

  given localDateDecoder: Decoder[LocalDate] = Decoder.decodeString.emap[LocalDate](str => {
    Either.catchNonFatal(LocalDate.parse(str, formatter)).leftMap(_.getMessage)
  })

  given zonedDateTimeEncoder: Encoder[ZonedDateTime] =
    Encoder.encodeString.contramap[ZonedDateTime](_.format(DateTimeFormatter.ISO_ZONED_DATE_TIME))

  given zonedDateTimeDecoder: Decoder[ZonedDateTime] = Decoder.decodeString.emap[ZonedDateTime](str => {
    Either.catchNonFatal(ZonedDateTime.parse(str, DateTimeFormatter.ISO_ZONED_DATE_TIME)).leftMap(_.getMessage)
  })
}
