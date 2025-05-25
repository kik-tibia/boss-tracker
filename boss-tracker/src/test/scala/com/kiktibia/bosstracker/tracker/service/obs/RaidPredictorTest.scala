import munit.FunSuite
import java.time._
import java.util.UUID
import com.kiktibia.bosstracker.tracker.service.obs.RaidPredictor
import com.kiktibia.bosstracker.tracker.service.obs.ObsModel.RaidWithCandidates
import com.kiktibia.bosstracker.tracker.repo.RaidDto
import com.kiktibia.bosstracker.tracker.repo.RaidTypeDto

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
}
