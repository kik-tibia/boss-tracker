package com.kiktibia.bosstracker.tracker.repo

import java.time.LocalDate
import java.time.OffsetDateTime
import java.util.UUID

case class DiscordMessageRow(
    id: Long,
    createdAt: OffsetDateTime,
    messageType: String,
    guildId: String,
    channelId: String,
    messageId: String
)

case class RaidTypeRow(
    id: Long,
    name: String,
    message: String,
    area: Option[String],
    subarea: Option[String],
    windowMin: Option[Int],
    windowMax: Option[Int],
    eventStart: Option[LocalDate],
    eventEnd: Option[LocalDate]
)

case class RaidRow(
    raidId: UUID,
    raidTypeId: Option[Long],
    area: Option[String],
    subarea: Option[String],
    startDate: OffsetDateTime
)

case class RaidTypeDto(
    id: Long,
    name: String,
    message: String,
    area: Option[String],
    subarea: Option[String],
    windowMin: Option[Int],
    windowMax: Option[Int],
    eventStart: Option[LocalDate],
    eventEnd: Option[LocalDate]
)

case class RaidDto(
    raidId: UUID,
    raidType: Option[RaidTypeDto],
    area: Option[String],
    subarea: Option[String],
    startDate: OffsetDateTime
)

case class DiscordMessageDto(
    id: Long,
    createdAt: OffsetDateTime,
    messageType: String,
    guildId: String,
    channelId: String,
    messageId: String
)
