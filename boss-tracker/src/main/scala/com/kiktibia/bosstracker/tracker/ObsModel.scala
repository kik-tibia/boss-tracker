package com.kiktibia.bosstracker.tracker

import java.time.ZonedDateTime
import java.util.UUID

object ObsModel {

  case class Raid(
      raidId: UUID,
      raidTypeId: Int,
      announcementDate: ZonedDateTime,
      startDate: ZonedDateTime,
      category: String,
      worldName: String,
      areaName: Option[String],
      subareaName: Option[String],
      ruleIds: List[String]
  )

}
