package com.kiktibia.bosstracker.tracker

import java.time.ZonedDateTime

object ObsModel {

  case class Raid(
      raidId: String,
      raidTypeId: Int,
      announcementDate: ZonedDateTime,
      startDate: ZonedDateTime,
      category: String,
      worldName: String,
      areaName: String,
      subareaName: Option[String],
      ruleIds: List[String]
  )

}
