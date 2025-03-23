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

  object Raid {
    given Ordering[Raid] with
      def compare(a: Raid, b: Raid): Int = {
        if (a.announcementDate.isAfter(b.announcementDate)) 1
        else if (a.announcementDate.isBefore(b.announcementDate)) -1
        else if (a.areaName.isDefined && b.areaName.isEmpty) 1
        else if (a.areaName.isEmpty && b.areaName.isDefined) -1
        else if (a.subareaName.isDefined && b.subareaName.isEmpty) 1
        else if (a.subareaName.isEmpty && b.subareaName.isDefined) -1
        else 0
      }
  }

}
