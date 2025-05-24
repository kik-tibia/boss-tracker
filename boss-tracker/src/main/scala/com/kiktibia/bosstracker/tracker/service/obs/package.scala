package com.kiktibia.bosstracker.tracker.service.obs

import java.time.LocalTime
import java.time.ZoneId
import java.time.ZonedDateTime

val zone = ZoneId.of("Europe/Berlin")

def zdtToSS(zdt: ZonedDateTime) = {
  val zdtAt10am = ZonedDateTime.of(zdt.toLocalDate, LocalTime.of(10, 0), zone)
  if (zdt.isBefore(zdtAt10am)) zdtAt10am.minusDays(1) else zdtAt10am
}
