package com.kiktibia.bosstracker.tracker.service.obs

import com.kiktibia.bosstracker.tracker.repo.RaidDto
import com.kiktibia.bosstracker.tracker.service.obs.ObsModel.CandidateProbability
import com.kiktibia.bosstracker.tracker.service.obs.ObsModel.RaidWithCandidates
import com.kiktibia.bosstracker.tracker.service.obs.ObsModel.RaidWithProbabilities

import java.time.LocalDate
import java.time.LocalTime
import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import com.kiktibia.bosstracker.tracker.repo.RaidTypeDto
import java.time.OffsetDateTime

object RaidPredictor {

  case class RaidChance(chance: Double, isLost: Boolean = false)

  def calculateProbabilities(raid: RaidWithCandidates): RaidWithProbabilities = {
    val candidatesWithTimeLeft =
      raid.candidates.flatMap(c => calculateInstantaneousChance(c, raid.raid.startDate).map(i => (c, i)))
    // To work out the probability that a raid occurs first out of all other events, we use this formula
    // P_i is the probability that raid i occurs before all other raids (what we are trying to calculate)
    // p_i is the instantaneous chance
    // P_i = p_i / (p_1 + p_2 + ... p_n)
    // And to avoid any issues with floating point precision for small numbers, we just multiply each p_i by a large number (timeSum)
    val timeSum = candidatesWithTimeLeft.map(_._2.chance).sum
    val denominator = candidatesWithTimeLeft.map(c => timeSum / c._2.chance).sum
    val probabilities = candidatesWithTimeLeft
      .map { case (raidType, raidChance) =>
        CandidateProbability(raidType, (timeSum / raidChance.chance) / denominator, raidChance.isLost)
      }
      .sortBy(-_.probability)
    RaidWithProbabilities(raid.raid, probabilities)
  }

  /** Calculates the instantaneous chance of this raid happening in the next second. This is typically just the number
    * of seconds left in the raid's window, but is weighted to take into account lower probabilities of the raid
    * happening in the first and last days of the window. Returns the reciprocal, e.g. a raid with 5% chance to occur in
    * the next second returns 20.
    */
  def calculateInstantaneousChance(raidType: RaidTypeDto, raidStart: OffsetDateTime): Option[RaidChance] = {
    val raidStartZdt: ZonedDateTime = raidStart.toInstant.atZone(zone)
    val currentSS: ZonedDateTime = zdtToSS(raidStartZdt)

    def chanceForNormalRaid(
      lastZdt: ZonedDateTime,
      maybeDuration: Option[Int],
      ssOfLast: ZonedDateTime,
      windowEnd: ZonedDateTime,
      windowStart: ZonedDateTime
    ): Double = {
      // The integer number of days remaining in the window
      val daysInWindow = ChronoUnit.DAYS.between(currentSS.toLocalDate, windowEnd.toLocalDate)
      // The fraction (0 to 1) into the day that the last raid occurred - e.g. 16:00 = 0.25, 06:00 = 0.8333...
      val lastRaidFractionIntoDay = ChronoUnit.SECONDS.between(ssOfLast, lastZdt) / 86400.0
      // The total number of seconds left in the window across all days, taking into account hours the raid can't occur (close to SS)
      val secondsInWindow = (ChronoUnit.SECONDS.between(raidStartZdt, windowEnd)
        - (daysInWindow * maybeDuration.getOrElse(1) * 3600)).toDouble

      // If a raid occurs shortly after SS, the first day of the window is higher chance than the last day of the window
      // Conversely, if a raid occurs shortly before SS, the last day of the window is higher chance than the first day
      // These chances approach 0% and 100% depending on how close to SS the raid occurred
      // The following statement takes this into account
      val weightedSecondsInWindow =
        if (currentSS.plusDays(1) == windowEnd) secondsInWindow
        else {
          val weightedEndSecondsInWindow =
            secondsInWindow - (1 - lastRaidFractionIntoDay) * (86400 - maybeDuration.getOrElse(1) * 3600)
          if (currentSS == windowStart) {
            weightedEndSecondsInWindow / (1 - lastRaidFractionIntoDay)
          } else {
            weightedEndSecondsInWindow
          }
        }
      weightedSecondsInWindow
    }

    // This case is for event raids, for the first raid since the start of the event
    // It's not mathematically correct but doesn't matter too much
    def chanceForFirstInEvent(
      eventStart: LocalDate,
      maybeDuration: Option[Int],
      windowMin: Int,
      windowMax: Int
    ): Double = {
      val eventStartFull =
        ZonedDateTime.of(eventStart.withYear(raidStartZdt.getYear), LocalTime.of(10, 0), zone)
      val assumedWindowEnd = eventStartFull.plusDays((windowMax - windowMin + 1))
      val daysInWindow = ChronoUnit.DAYS.between(eventStartFull, assumedWindowEnd)
      if (raidStartZdt.isBefore(assumedWindowEnd))
        ChronoUnit.SECONDS.between(
          raidStartZdt,
          assumedWindowEnd
        ) - (daysInWindow * maybeDuration.getOrElse(1) * 3600.0)
      else ChronoUnit.SECONDS.between(
        eventStartFull,
        assumedWindowEnd
      ) - (daysInWindow * maybeDuration.getOrElse(1) * 3600.0)
    }

    def chanceForLostRaid(maybeDuration: Option[Int], windowMin: Int, windowMax: Int): Double =
      (windowMin + windowMax).toDouble / 2 * (86400 - maybeDuration.getOrElse(1) * 3600)

    raidType match {
      // The raid type is an event raid but we are outside the event
      case HasEventWindow(eventStart, eventEnd) if !insideEvent(raidStartZdt, eventStart, eventEnd) => None
      // The raid type has a duration but the time is too close to SS for it to occur
      case HasDuration(duration) if ChronoUnit.HOURS.between(raidStartZdt, currentSS.plusDays(1)) < duration => None
      // The raid type might be possible to occur, calculate the chance
      case HasLastOccurrenceAndWindow(lastOccurrence, windowMin, windowMax) =>
        val lastZdt = lastOccurrence.toInstant.atZone(zone)
        val ssOfLast = zdtToSS(lastZdt)
        val windowStart = ssOfLast.plusDays(windowMin)
        val windowEnd = ssOfLast.plusDays(windowMax + 1)

        if (raidStartZdt.isBefore(windowStart)) None
        else if (raidStartZdt.isAfter(windowEnd)) {
          raidType.eventStart match {
            case Some(eventStart) =>
              Some(RaidChance(chanceForFirstInEvent(eventStart, raidType.duration, windowMin, windowMax)))
            case None =>
              if (raidStartZdt.isAfter(windowStart.plusDays(windowMin)))
                Some(RaidChance(chanceForLostRaid(raidType.duration, windowMin, windowMax), isLost = true))
              else None
          }
        } else Some(RaidChance(chanceForNormalRaid(lastZdt, raidType.duration, ssOfLast, windowEnd, windowStart)))
      // The raid type has never occurred in database history
      case HasWindowButNoLastOccurrence(windowMin, windowMax) =>
        raidType.eventStart match {
          case Some(eventStart) =>
            Some(RaidChance(chanceForFirstInEvent(eventStart, raidType.duration, windowMin, windowMax)))
          case None => Some(RaidChance(chanceForLostRaid(raidType.duration, windowMin, windowMax), isLost = true))
        }
      case _ => None
    }
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

  private def insideEvent(start: ZonedDateTime, eventStart: LocalDate, eventEnd: LocalDate): Boolean = {
    val zonedEventStart = ZonedDateTime.of(eventStart.withYear(start.getYear), LocalTime.of(10, 0), zone)
    val zonedEventEnd = ZonedDateTime.of(eventEnd.withYear(start.getYear), LocalTime.of(10, 0), zone)
    !start.isBefore(zonedEventStart) && !start.isAfter(zonedEventEnd)
  }

  private object HasEventWindow {
    def unapply(raidType: RaidTypeDto): Option[(LocalDate, LocalDate)] =
      for
        eventStart <- raidType.eventStart
        eventEnd <- raidType.eventEnd
      yield (eventStart, eventEnd)
  }
  private object HasDuration {
    def unapply(raidType: RaidTypeDto): Option[Int] =
      raidType.duration
  }
  private object HasLastOccurrenceAndWindow {
    def unapply(raidType: RaidTypeDto): Option[(OffsetDateTime, Int, Int)] =
      for
        lastOccurrence <- raidType.lastOccurrence
        windowMin <- raidType.windowMin
        windowMax <- raidType.windowMax
      yield (lastOccurrence, windowMin, windowMax)
  }
  private object HasWindowButNoLastOccurrence {
    def unapply(raidType: RaidTypeDto): Option[(Int, Int)] =
      for
        windowMin <- raidType.windowMin
        windowMax <- raidType.windowMax
        if raidType.lastOccurrence.isEmpty
      yield (windowMin, windowMax)
  }
}
