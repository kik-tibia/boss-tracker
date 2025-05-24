package com.kiktibia.bosstracker.tracker.service.obs

import com.kiktibia.bosstracker.tracker.repo.RaidDto
import com.kiktibia.bosstracker.tracker.service.obs.ObsModel.CandidateProbability
import com.kiktibia.bosstracker.tracker.service.obs.ObsModel.RaidWithCandidates
import com.kiktibia.bosstracker.tracker.service.obs.ObsModel.RaidWithProbabilities

import java.time.LocalDate
import java.time.LocalTime
import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

object RaidPredictor {

  def calculateProbabilities(raid: RaidWithCandidates): RaidWithProbabilities = {
    val raidStart: ZonedDateTime = raid.raid.startDate.toInstant.atZone(zone)
    val currentSS: ZonedDateTime = zdtToSS(raidStart)
    val candidatesWithTimeLeft = raid.candidates.flatMap { c =>
      (c.lastOccurrence, c.windowMin, c.windowMax, c.duration, c.eventStart, c.eventEnd) match {
        case (_, _, _, _, Some(eventStart), Some(eventEnd)) if !insideEvent(raidStart, eventStart, eventEnd) => None
        case (_, _, _, Some(duration), _, _) if ChronoUnit.HOURS.between(raidStart, currentSS.plusDays(1)) < duration =>
          None
        case (Some(lastOccurrence), Some(windowMin), Some(windowMax), maybeDuration, _, _) =>
          val lastZdt = lastOccurrence.toInstant.atZone(zone)
          val ssOfLast = zdtToSS(lastZdt)
          val windowStart = ssOfLast.plusDays(windowMin)
          val windowEnd = ssOfLast.plusDays(windowMax + 1)

          if (raidStart.isBefore(windowStart) || raidStart.isAfter(windowEnd)) {
            c.eventStart match {
              case None => None
              case Some(eventStart) => // This case is for event raids, for the first raid since the start of the event
                val eventStartFull = ZonedDateTime.of(eventStart.withYear(raidStart.getYear), LocalTime.of(10, 0), zone)
                val assumedWindowEnd = eventStartFull.plusDays((windowMax - windowMin + 1))
                Some(c, ChronoUnit.SECONDS.between(raidStart, assumedWindowEnd) - (maybeDuration.getOrElse(1) * 3600.0))
            }
          } else {
            // The integer number of days remaining in the window
            val daysInWindow = ChronoUnit.DAYS.between(currentSS.toLocalDate, windowEnd.toLocalDate)
            // The fraction (0 to 1) into the day that the last raid occurred - e.g. 16:00 = 0.25, 06:00 = 0.8333...
            val lastRaidFractionIntoDay = ChronoUnit.SECONDS.between(ssOfLast, lastZdt) / 86400.0
            // The total number of seconds left in the window across all days, taking into account hours the raid can't occur (close to SS)
            val secondsInWindow = (ChronoUnit.SECONDS.between(raidStart, windowEnd)
              - (daysInWindow * maybeDuration.getOrElse(1) * 3600)).toDouble
            // If a raid occurs shortly after SS, the first day of the window is higher chance than the last day of the window
            // Conversely, if a raid occurs shortly before SS, the last day of the window is higher chance than the first day
            // These chances approach 0% and 100% depending on how close to SS the raid occurred
            // The following two vals (weighted seconds) take this into account
            val weightedEndSecondsInWindow =
              if (currentSS.plusDays(1) == windowEnd) secondsInWindow
              else secondsInWindow - (1 - lastRaidFractionIntoDay) * (86400 - maybeDuration.getOrElse(1) * 3600)
            val weightedStartEndSecondsInWindow =
              if (currentSS == windowStart) weightedEndSecondsInWindow / (1 - lastRaidFractionIntoDay)
              else weightedEndSecondsInWindow

            Some(c, weightedStartEndSecondsInWindow)
          }
        case (None, Some(windowMin), Some(windowMax), maybeDuration, _, _) =>
          // Raid has never occurred in database history
          c.eventStart match {
            case None =>
              Some(c, windowMax.toDouble * (86400 - maybeDuration.getOrElse(1) * 3600))
            case Some(eventStart) => // This case is for event raids, for the first raid since the start of the event
              val eventStartFull = ZonedDateTime.of(eventStart.withYear(raidStart.getYear), LocalTime.of(10, 0), zone)
              val assumedWindowEnd = eventStartFull.plusDays((windowMax - windowMin + 1))
              Some(c, ChronoUnit.SECONDS.between(raidStart, assumedWindowEnd) - (maybeDuration.getOrElse(1) * 3600.0))
          }

        case _ => None
      }
    }
    // Now we have the time left, in seconds, for each candidate raid
    // This time is weighted to take into account lower probabilities for the start and end of the window
    // The reciprocal of this time can be thought of as the instantaneous chance that a raid will occur at that exact second
    // So to work out the probability that a raid occurs first out of all other events, we use this formula
    // P_i is the probability that raid i occurs first
    // p_i is the instantanous chance above (i.e. the reciprocal of the weighted window)
    // P_i = p_i / (p_1 + p_2 + ... p_n)
    // And to avoid any issues with floating point precision for small numbers, we just multiply each p_i by a large number (timeSum)
    val timeSum = candidatesWithTimeLeft.map(_._2).sum
    val denominator = candidatesWithTimeLeft.map(c => timeSum / c._2).sum
    val probabilities = candidatesWithTimeLeft
      .map { c =>
        CandidateProbability(c._1, (timeSum / c._2) / denominator)
      }
      .sortBy(-_.probability)
    RaidWithProbabilities(raid.raid, probabilities)
  }

  def logProbabilities(raids: List[RaidWithProbabilities]): Unit = {
    println("--- Probabilities ---")
    raids.filter(_.probabilities.nonEmpty).foreach { raid =>
      println(raid.raid.startDate)
      raid.probabilities.sortBy(-_.probability).foreach { p =>
        println(f"${p.probability * 100}%.2f%% - ${p.raidType.name}")
      }
    }
  }

  def noteworthyRaidWarnings(raids: List[RaidDto]): List[String] = {
    raids.flatMap { raid =>
      if (raid.area.contains("edron") && raid.subarea.isEmpty) Some("Edron raid")
      else None
    }
  }

  private def insideEvent(start: ZonedDateTime, eventStart: LocalDate, eventEnd: LocalDate): Boolean = {
    val zonedEventStart = ZonedDateTime.of(eventStart.withYear(start.getYear), LocalTime.of(10, 0), zone)
    val zonedEventEnd = ZonedDateTime.of(eventEnd.withYear(start.getYear), LocalTime.of(10, 0), zone)
    !start.isBefore(zonedEventStart) && !start.isAfter(zonedEventEnd)
  }
}
