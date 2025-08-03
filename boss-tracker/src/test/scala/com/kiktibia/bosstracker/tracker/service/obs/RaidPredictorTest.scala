import com.kiktibia.bosstracker.tracker.repo.RaidDto
import com.kiktibia.bosstracker.tracker.repo.RaidTypeDto
import com.kiktibia.bosstracker.tracker.service.obs.ObsModel.RaidWithCandidates
import com.kiktibia.bosstracker.tracker.service.obs.RaidPredictor
import munit.FunSuite

import java.time.*
import java.util.UUID

class RaidPredictorTest extends FunSuite {
  val defaultRaidDto = RaidDto(
    raidId = UUID.randomUUID(),
    raidType = None,
    area = None,
    subarea = None,
    startDate = OffsetDateTime.of(2025, 6, 15, 12, 0, 0, 0, ZoneOffset.UTC)
  )
  val defaultRaidTypeDto = RaidTypeDto(
    id = 0,
    name = "Raid",
    message = "A raid is happening",
    area = Some("Area"),
    subarea = Some("Subarea"),
    windowMin = Some(5),
    windowMax = Some(10),
    eventStart = None,
    eventEnd = None,
    duration = Some(4),
    rolePrefix = None,
    lastOccurrence = Some(OffsetDateTime.of(2025, 6, 8, 12, 0, 0, 0, ZoneOffset.UTC))
  )

  test("no candidates returns empty probabilities") {
    val rwc = RaidWithCandidates(defaultRaidDto, Nil)
    val result = RaidPredictor.calculateProbabilities(rwc)
    assertEquals(result.probabilities, Nil)
  }

  test("one candidate returns 1.0 probability") {
    val candidates = List(
      defaultRaidTypeDto
    )
    val rwc = RaidWithCandidates(defaultRaidDto, candidates)
    val result = RaidPredictor.calculateProbabilities(rwc)
    assertEquals(result.probabilities.map(_.probability), List(1.0))
  }

  test("two identical candidates return 0.5 probability each") {
    val candidates = List(
      defaultRaidTypeDto.copy(name = "Raid 1"),
      defaultRaidTypeDto.copy(name = "Raid 2")
    )
    val rwc = RaidWithCandidates(defaultRaidDto, candidates)
    val result = RaidPredictor.calculateProbabilities(rwc)
    val resultMap = result.probabilities.map(p => p.raidType.name -> p.probability).toMap
    assertEqualsDouble(resultMap("Raid 1"), 0.5, 1e-6)
    assertEqualsDouble(resultMap("Raid 2"), 0.5, 1e-6)
  }

  test("three candidates with 2h, 3h and 5h left in their windows are calculated correctly") {
    val raid = defaultRaidDto.copy(startDate = OffsetDateTime.of(2025, 6, 18, 18, 0, 0, 0, ZoneOffset.UTC))
    val candidates = List(
      defaultRaidTypeDto.copy(name = "Raid 1", duration = Some(12)), // 2h left in window
      defaultRaidTypeDto.copy(name = "Raid 2", duration = Some(11)), // 3h left in window
      defaultRaidTypeDto.copy(name = "Raid 3", duration = Some(9)) // 5h left in window
    )
    val rwc = RaidWithCandidates(raid, candidates)
    val result = RaidPredictor.calculateProbabilities(rwc)
    val resultMap = result.probabilities.map(p => p.raidType.name -> p.probability).toMap
    assertEqualsDouble(resultMap("Raid 1"), (1 / 2.0) / (1 / 2.0 + 1 / 3.0 + 1 / 5.0), 1e-6)
    assertEqualsDouble(resultMap("Raid 2"), (1 / 3.0) / (1 / 2.0 + 1 / 3.0 + 1 / 5.0), 1e-6)
    assertEqualsDouble(resultMap("Raid 3"), (1 / 5.0) / (1 / 2.0 + 1 / 3.0 + 1 / 5.0), 1e-6)
  }

  test("instantaneous chance is calculated correctly for a raid in its last day") {
    val raidType =
      defaultRaidTypeDto.copy(lastOccurrence = Some(OffsetDateTime.of(2025, 6, 8, 14, 0, 0, 0, ZoneOffset.UTC)))
    val raidStart = OffsetDateTime.of(2025, 6, 18, 22, 0, 0, 0, ZoneOffset.UTC)
    val expected = Some(3600 * 6)
    val chance = RaidPredictor
      .calculateInstantaneousChance(raidType, raidStart).map(_.chance)
    assertOptionDouble(chance, expected)
  }

  test("instantaneous chance is calculated correctly for a raid in the middle of its window") {
    val raidType =
      defaultRaidTypeDto.copy(lastOccurrence = Some(OffsetDateTime.of(2025, 6, 10, 14, 0, 0, 0, ZoneOffset.UTC)))
    val raidStart = OffsetDateTime.of(2025, 6, 18, 22, 0, 0, 0, ZoneOffset.UTC)
    val expected = Some(3600 * 6 + 3600 * 20 + 3600 * 20 / 4)
    val chance = RaidPredictor
      .calculateInstantaneousChance(raidType, raidStart).map(_.chance)
    assertOptionDouble(chance, expected)
  }

  test("instantaneous chance is calculated correctly for a raid in its first day") {
    val raidType =
      defaultRaidTypeDto.copy(lastOccurrence = Some(OffsetDateTime.of(2025, 6, 10, 14, 0, 0, 0, ZoneOffset.UTC)))
    val raidStart = OffsetDateTime.of(2025, 6, 15, 22, 0, 0, 0, ZoneOffset.UTC)
    val expected = Some((3600 * 6 + 3600 * 20 * 4 + 3600 * 20 / 4) / 0.75)
    val chance = RaidPredictor
      .calculateInstantaneousChance(raidType, raidStart).map(_.chance)
    assertOptionDouble(chance, expected)
  }

  test("instantaneous chance is calculated correctly for a raid that has a one day window") {
    val raidType =
      defaultRaidTypeDto.copy(
        windowMin = Some(5),
        windowMax = Some(5),
        lastOccurrence = Some(OffsetDateTime.of(2025, 6, 10, 14, 0, 0, 0, ZoneOffset.UTC))
      )
    val raidStart = OffsetDateTime.of(2025, 6, 15, 22, 0, 0, 0, ZoneOffset.UTC)
    val expected = Some(3600 * 6)
    val chance = RaidPredictor
      .calculateInstantaneousChance(raidType, raidStart).map(_.chance)
    assertOptionDouble(chance, expected)
  }

  test(
    "instantaneous chance is calculated correctly for a raid that has a one day window and that has already occurred today"
  ) {
    val raidType =
      defaultRaidTypeDto.copy(
        windowMin = Some(1),
        windowMax = Some(1),
        eventStart = Some(LocalDate.of(2000, 6, 1)),
        eventEnd = Some(LocalDate.of(2000, 7, 1)),
        lastOccurrence = Some(OffsetDateTime.of(2025, 6, 10, 14, 0, 0, 0, ZoneOffset.UTC))
      )
    val raidStart = OffsetDateTime.of(2025, 6, 10, 22, 0, 0, 0, ZoneOffset.UTC)
    val expected = None
    val chance = RaidPredictor
      .calculateInstantaneousChance(raidType, raidStart).map(_.chance)
    assertOptionDouble[Double](chance, expected)
  }

  test("instantaneous chance returns None if it's too close to SS for the raid's duration") {
    val raidType =
      defaultRaidTypeDto.copy(lastOccurrence = Some(OffsetDateTime.of(2025, 6, 10, 14, 0, 0, 0, ZoneOffset.UTC)))
    val raidStart = OffsetDateTime.of(2025, 6, 19, 6, 0, 0, 0, ZoneOffset.UTC)
    val expected = None
    val chance = RaidPredictor
      .calculateInstantaneousChance(raidType, raidStart).map(_.chance)
    assertOptionDouble[Double](chance, expected)
  }

  test("instantaneous chance is just zero to average window for raids that have no recorded history") {
    val raidType =
      defaultRaidTypeDto.copy(
        windowMin = Some(4),
        windowMax = Some(10),
        lastOccurrence = None
      )
    val raidStart = OffsetDateTime.of(2025, 6, 15, 22, 0, 0, 0, ZoneOffset.UTC)
    val expected = Some(3600 * 20 * 7)
    val chance = RaidPredictor
      .calculateInstantaneousChance(raidType, raidStart).map(_.chance)
    assertOptionDouble(chance, expected)
  }

  test("instantaneous chance is calculated correctly for an event raid if the event is active") {
    val raidType =
      defaultRaidTypeDto.copy(
        windowMin = Some(1),
        windowMax = Some(2),
        eventStart = Some(LocalDate.of(2000, 6, 1)),
        eventEnd = Some(LocalDate.of(2000, 7, 1)),
        lastOccurrence = Some(OffsetDateTime.of(2025, 6, 10, 14, 0, 0, 0, ZoneOffset.UTC)),
        duration = Some(1)
      )
    val raidStart = OffsetDateTime.of(2025, 6, 12, 22, 0, 0, 0, ZoneOffset.UTC)
    val expected = Some(3600 * 9)
    val chance = RaidPredictor
      .calculateInstantaneousChance(raidType, raidStart).map(_.chance)
    assertOptionDouble(chance, expected)
  }

  test(
    "instantaneous chance is calculated correctly for an event raid if the event is active and the first was missed"
  ) {
    val raidType =
      defaultRaidTypeDto.copy(
        windowMin = Some(1),
        windowMax = Some(2),
        eventStart = Some(LocalDate.of(2000, 6, 1)),
        eventEnd = Some(LocalDate.of(2000, 7, 1)),
        lastOccurrence = None,
        duration = Some(1)
      )
    val raidStart = OffsetDateTime.of(2025, 6, 12, 22, 0, 0, 0, ZoneOffset.UTC)
    val expected = Some(86400 * 2 - 7200)
    val chance = RaidPredictor
      .calculateInstantaneousChance(raidType, raidStart).map(_.chance)
    assertOptionDouble(chance, expected)
  }

  test("instantaneous chance returns None for an event raid if the event is inactive") {
    val raidType =
      defaultRaidTypeDto.copy(
        windowMin = Some(1),
        windowMax = Some(2),
        eventStart = Some(LocalDate.of(2000, 7, 1)),
        eventEnd = Some(LocalDate.of(2000, 8, 1)),
        lastOccurrence = Some(OffsetDateTime.of(2025, 6, 10, 14, 0, 0, 0, ZoneOffset.UTC)),
        duration = Some(1)
      )
    val raidStart = OffsetDateTime.of(2025, 6, 12, 22, 0, 0, 0, ZoneOffset.UTC)
    val expected = None
    val chance = RaidPredictor
      .calculateInstantaneousChance(raidType, raidStart).map(_.chance)
    assertOptionDouble[Double](chance, expected)
  }

  test("instantaneous chance is calculated correctly for an event raid if it's the first raid of the event") {
    val raidType =
      defaultRaidTypeDto.copy(
        windowMin = Some(1),
        windowMax = Some(2),
        eventStart = Some(LocalDate.of(2000, 6, 1)),
        eventEnd = Some(LocalDate.of(2000, 7, 1)),
        lastOccurrence = Some(OffsetDateTime.of(2024, 6, 30, 14, 0, 0, 0, ZoneOffset.UTC)),
        duration = Some(1)
      )
    val raidStart = OffsetDateTime.of(2025, 6, 1, 22, 0, 0, 0, ZoneOffset.UTC)
    val expected = Some(3600 * 9 + 3600 * 23)
    val chance = RaidPredictor
      .calculateInstantaneousChance(raidType, raidStart).map(_.chance)
    assertOptionDouble(chance, expected)
  }

  test("instantaneous chance is calculated correctly for an event raid if there is no recorded history") {
    val raidType =
      defaultRaidTypeDto.copy(
        windowMin = Some(1),
        windowMax = Some(2),
        eventStart = Some(LocalDate.of(2000, 6, 1)),
        eventEnd = Some(LocalDate.of(2000, 7, 1)),
        lastOccurrence = None,
        duration = Some(1)
      )
    val raidStart = OffsetDateTime.of(2025, 6, 1, 22, 0, 0, 0, ZoneOffset.UTC)
    val expected = Some(3600 * 9 + 3600 * 23)
    val chance = RaidPredictor
      .calculateInstantaneousChance(raidType, raidStart).map(_.chance)
    assertOptionDouble(chance, expected)
  }

  test("instantaneous chance is calculated correctly for lost raids in their second window") {
    val raidType =
      defaultRaidTypeDto.copy(
        windowMin = Some(8),
        windowMax = Some(10),
        lastOccurrence = Some(OffsetDateTime.of(2025, 6, 2, 14, 0, 0, 0, ZoneOffset.UTC)),
        duration = Some(1)
      )
    val raidStart = OffsetDateTime.of(2025, 6, 20, 16, 0, 0, 0, ZoneOffset.UTC)
    val expected = Some((86400 - 3600) * 9)
    val chance = RaidPredictor
      .calculateInstantaneousChance(raidType, raidStart).map(_.chance)
    assertOptionDouble(chance, expected)
  }

  test(
    "instantaneous chance is calculated correctly for lost raids that haven't reached their second window chance yet"
  ) {
    val raidType =
      defaultRaidTypeDto.copy(
        windowMin = Some(8),
        windowMax = Some(10),
        lastOccurrence = Some(OffsetDateTime.of(2025, 6, 2, 14, 0, 0, 0, ZoneOffset.UTC)),
        duration = Some(1)
      )
    val raidStart = OffsetDateTime.of(2025, 6, 15, 16, 0, 0, 0, ZoneOffset.UTC)
    val expected = None
    val chance = RaidPredictor
      .calculateInstantaneousChance(raidType, raidStart).map(_.chance)
    assertOptionDouble[Double](chance, expected)
  }

  private def assertOptionDouble[T: Numeric](obtained: Option[Double], expected: Option[T]) = {
    val num = implicitly[Numeric[T]]
    (obtained, expected.map(num.toDouble)) match {
      case (Some(o), Some(e)) => assertEqualsDouble(o, e, 1e-6)
      case (None, None) => ()
      case _ => fail(s"Expected $expected, got $obtained")
    }
  }
}
