package com.kiktibia.bosstracker.tracker.repo

import cats.effect.IO
import cats.implicits.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.util.transactor.Transactor
import doobie.util.update.Update

import java.time.OffsetDateTime
import java.util.UUID

class BossTrackerRepo(tx: Transactor[IO]) {

  def getDiscordMessages(messageType: String, after: OffsetDateTime): IO[List[DiscordMessageDto]] = {
    sql"""
      SELECT id, created_at, message_type, guild_id, channel_id, message_id
      FROM discord_message
      WHERE message_type = $messageType
      AND created_at >= $after
    """.query[DiscordMessageDto].to[List].transact(tx)
  }

  def upsertDiscordMessages(discordMessages: List[DiscordMessageDto]): IO[Int] = {
    val sql =
      """INSERT INTO discord_message(created_at, message_type, guild_id, channel_id, message_id)
        VALUES (?, ?, ?, ?, ?)
        ON CONFLICT (message_type, guild_id)
        DO UPDATE SET created_at = EXCLUDED.created_at, channel_id = EXCLUDED.channel_id, message_id = EXCLUDED.message_id
      """
    Update[(OffsetDateTime, String, String, String, String)](sql)
      .updateMany(discordMessages.map(m => (m.createdAt, m.messageType, m.guildId, m.channelId, m.messageId)))
      .transact(tx)
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
              SELECT id, name, message, area, subarea, window_min, window_max, event_start, event_end, duration, role_prefix
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

  def getOtherRaidsStage1(area: String): IO[List[RaidTypeDto]] = {
    sql"""
      SELECT rt.*, MAX(r.start_date)
      FROM raid_type rt
      LEFT JOIN raid r ON r.raid_type_id = rt.id
      WHERE rt.area = $area
      GROUP BY rt.id
    """.query[RaidTypeDto].to[List].transact(tx)
  }

  def getOtherRaidsStage2(area: String, subarea: String): IO[List[RaidTypeDto]] = {
    sql"""
      SELECT rt.*, MAX(r.start_date)
      FROM raid_type rt
      LEFT JOIN raid r ON r.raid_type_id = rt.id
      WHERE rt.area = $area
      AND (
           ($subarea = 'None' AND rt.subarea IS NULL)
        OR ($subarea <> 'None' AND rt.subarea = $subarea)
      )
      GROUP BY rt.id
    """.query[RaidTypeDto].to[List].transact(tx)
  }

  def getPreviousRaidOfSameType(raid_id: UUID): IO[Option[RaidRow]] = {
    sql"""
      WITH TARGET AS
        (SELECT raid_type_id, start_date FROM raid WHERE raid_id = $raid_id)
      SELECT r2.*
      FROM raid r2
      JOIN TARGET t on r2.raid_type_id = t.raid_type_id
      WHERE r2.start_date < t.start_date
      ORDER BY r2.start_date DESC LIMIT 1
    """.query[RaidRow].option.transact(tx)
  }

  def getRolePrefixes(): IO[List[String]] = {
    sql"""
    SELECT role_prefix
    FROM raid_type
    WHERE role_prefix IS NOT NULL
    """.query[String].to[List].transact(tx)
  }

  // Opens multiple connections for each uuid but there should never be more than 10 or 20
  def getRaids(uuids: List[UUID]): IO[List[RaidDto]] =
    uuids.traverse(getRaid).map(_.flatten)

  def raidToDto(r: RaidRow, rt: Option[RaidTypeRow]): RaidDto =
    RaidDto(r.raidId, rt.map(raidTypeToDto), r.area, r.subarea, r.startDate)

  def raidTypeToDto(rt: RaidTypeRow): RaidTypeDto =
    RaidTypeDto(
      rt.id,
      rt.name,
      rt.message,
      rt.area,
      rt.subarea,
      rt.windowMin,
      rt.windowMax,
      rt.eventStart,
      rt.eventEnd,
      rt.duration,
      rt.rolePrefix,
      None
    )

}
