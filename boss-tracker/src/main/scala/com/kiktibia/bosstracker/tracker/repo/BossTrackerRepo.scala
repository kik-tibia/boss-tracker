package com.kiktibia.bosstracker.tracker.repo

import doobie.util.transactor.Transactor
import doobie.implicits.*
import doobie.postgres.implicits.*
import cats.effect.IO
import java.util.UUID

class BossTrackerRepo(tx: Transactor[IO]) {

  def selectOneThing(): IO[Unit] = {
    sql"SELECT name FROM raid_type".query[String].to[List].transact(tx).map { names =>
      println(names)
    }
  }

  def upsertRaid(raid: RaidRow): IO[Int] = {
    sql"""INSERT INTO raid(raid_id, raid_type_id, area, subarea, start_date)
      VALUES (${raid.raidId}, ${raid.raidTypeId}, ${raid.area}, ${raid.subarea}, ${raid.startDate})
      ON CONFLICT (raid_id)
      DO UPDATE
        SET
        raid_type_id = EXCLUDED.raid_type_id,
        area = EXCLUDED.area,
        subarea = EXCLUDED.subarea,
        start_date = EXCLUDED.start_date
    """.update.run
      .transact(tx)
  }

  // rt.id, rt.name, rt.message, rt.area, rt.subarea, rt.window_min, rt.window_max, rt.event_start, rt.event_end
  def getRaid(uuid: UUID): IO[Option[RaidDto]] = {
    (for {
      maybeRaid <- sql"""
      SELECT raid_id, raid_type_id, area, subarea, start_date
      FROM raid
      WHERE raid_id = $uuid
    """.query[RaidRow].option.transact(tx)
      result <- maybeRaid match {
        case Some(raid) =>
          raid.raidTypeId match {
            case Some(rtId) =>
              sql"""
              SELECT id, name, message, area, subarea, window_min, window_max, event_start, event_end
              FROM raid_type
              WHERE id = $rtId
            """.query[RaidTypeRow].option.transact(tx).map(rtOpt => Some((raid, rtOpt)))
            case None =>
              IO.pure(Some((raid, None)))
          }
        case None => IO.pure(None)
      }
    } yield result)
      .map(_.map(raidToDto))
  }

  def raidToDto(r: RaidRow, rt: Option[RaidTypeRow]): RaidDto =
    RaidDto(r.raidId, rt.map(raidTypeToDto), r.area, r.subarea, r.startDate)

  def raidTypeToDto(rt: RaidTypeRow): RaidTypeDto =
    RaidTypeDto(rt.id, rt.name, rt.message, rt.area, rt.subarea, rt.windowMin, rt.windowMax, rt.eventStart, rt.eventEnd)

}
