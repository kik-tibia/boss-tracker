package com.kiktibia.bosstracker.tracker.service

import cats.data.EitherT
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import com.kiktibia.bosstracker.config.Config
import com.kiktibia.bosstracker.tracker.CirceCodecs
import com.kiktibia.bosstracker.tracker.Model.BossList
import com.kiktibia.bosstracker.tracker.Model.DateInfo
import com.kiktibia.bosstracker.tracker.Model.KillStatsDay
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.io.Source
import scala.jdk.CollectionConverters.*

import sys.process.*
import java.time.Instant

class FileIO(cfg: Config) extends CirceCodecs {

  private val bossListFileSource: Resource[IO, Source] =
    Resource.fromAutoCloseable(IO.blocking(Source.fromFile(cfg.file.bossListFileName)))

  private val dateInfoFileSource: Resource[IO, Source] =
    Resource.fromAutoCloseable(IO.blocking(Source.fromFile(cfg.file.dateInfoFileName)))

  private val statsDateFileSource: Resource[IO, Source] =
    Resource.fromAutoCloseable(IO.blocking(Source.fromFile(cfg.file.statsDateFileName)))

  private val predictionsDateFileSource: Resource[IO, Source] =
    Resource.fromAutoCloseable(IO.blocking(Source.fromFile(cfg.file.predictionsDateFileName)))

  private val lastMwcPostFileSource: Resource[IO, Source] =
    Resource.fromAutoCloseable(IO.blocking(Source.fromFile(cfg.file.lastMwcPostFileName)))

  private val raidFileSource: Resource[IO, Source] =
    Resource.fromAutoCloseable(IO.blocking(Source.fromFile(cfg.file.raidDataFile)))

  def parseBossFile(): EitherT[IO, Error, BossList] = {
    EitherT(
      readFileToString(bossListFileSource)
        .map(s => parser.decode[BossList](s))
    )
  }

  def parseAllHistoricStats(bossList: BossList): IO[List[KillStatsDay]] = {
    getAllStatsFiles().map(
      _.map(f => new String(Files.readAllBytes(f)))
        .map { s =>
          parser.decode[KillStatsDay](s).map(ksd => removeIrrelevantEntries(ksd, bossList))
        }
        .flatMap(_.toOption)
    )
  }

  def getDateInfo(): EitherT[IO, Error, DateInfo] = {
    EitherT(
      readFileToString(dateInfoFileSource)
        .map(s => parser.decode[DateInfo](s))
    )
  }

  def updateBossStatsRepo() = IO.blocking {
    val silentLogger = ProcessLogger(_ => (), _ => ())
    // Doing this instead of a git pull because sometimes the repo is force pushed to and diverges from the local branch
    Process(Seq("git", "fetch"), Some(new java.io.File(cfg.file.statsRepoPath)))
      .!(silentLogger)
    Process(Seq("git", "reset", "--hard", "origin/main"), Some(new java.io.File(cfg.file.statsRepoPath)))
      .!(silentLogger)
  }

  def getLastKilledUpdate(): IO[LocalDate] = {
    readFileToString(statsDateFileSource)
      .map(s => LocalDate.parse(s.trim, DateTimeFormatter.ISO_DATE))
  }

  def getLastPredictionsUpdate(): IO[LocalDate] = {
    readFileToString(predictionsDateFileSource)
      .map(s => LocalDate.parse(s.trim, DateTimeFormatter.ISO_DATE))
  }

  def updateDateFile(dateFile: String, date: LocalDate): IO[Unit] = IO.blocking {
    val path = Paths.get(dateFile)
    Files.write(path, date.toString().getBytes())
  }

  def parseLatestStats(): EitherT[IO, Error, KillStatsDay] = {
    EitherT(for
      files <- getAllStatsFiles()
      path = files.sortBy(_.getFileName()).reverse.head
      jsonString = new String(Files.readAllBytes(path))
      parsed = parser.decode[KillStatsDay](jsonString)
    yield parsed)
  }

  def getTimeOfLastMwcPost(): IO[ZonedDateTime] =
    readFileToString(lastMwcPostFileSource)
      .map(s => ZonedDateTime.parse(s.trim, DateTimeFormatter.ISO_DATE_TIME))

  def getTimeOfLastMwcChange(): IO[ZonedDateTime] = IO.blocking {
    val mwcHistoryFiles = pathToList(Paths.get(cfg.file.mwcHistoryPath))
    ZonedDateTime.parse(
      mwcHistoryFiles.map(_.getFileName()).max.toString().split("\\.").head,
      DateTimeFormatter.ISO_DATE_TIME
    )
  }

  def getMwcDetails(): IO[String] = IO.blocking {
    val mwcHistoryFiles = pathToList(Paths.get(cfg.file.mwcHistoryPath))
    val latestFile = mwcHistoryFiles.maxBy(_.getFileName())
    new String(Files.readAllBytes(latestFile))
  }

  def updateLastMwcPost(date: ZonedDateTime): IO[Unit] = IO.blocking {
    val path = Paths.get(cfg.file.lastMwcPostFileName)
    Files.write(path, date.format(DateTimeFormatter.ISO_DATE_TIME).getBytes())
  }

  def raidDataModifiedTime(): IO[Instant] =
    IO.blocking {
      val path = Paths.get(cfg.file.raidDataFile.toString)
      Files.getLastModifiedTime(path).toInstant
    }

  def parseRaidData(): IO[String] = {
    raidFileSource.use { source =>
      IO.blocking(source.mkString)
    }
  }

  private def readFileToString(source: Resource[IO, Source]): IO[String] =
    source.use { s =>
      IO.blocking(s.mkString)
    }

  // Removes any entries in the KillStatsDay that are not in the BossList
  // (e.g. most of the regular creatures) to use less memory
  private def removeIrrelevantEntries(ksd: KillStatsDay, bossList: BossList): KillStatsDay = {
    val updatedEntries = ksd.killstatistics.entries
      .filter(e => bossList.bosses.exists(b => b.raceName.getOrElse(b.name) == e.race))
    ksd.copy(killstatistics = ksd.killstatistics.copy(entries = updatedEntries))
  }

  private def pathToList(path: Path): List[Path] = {
    Files
      .list(path)
      .iterator()
      .asScala
      .toList
  }

  private def getAllStatsFiles(): IO[List[Path]] = IO.blocking {
    val path = Paths.get(cfg.file.statsRepoPath, "data", cfg.general.world)
    val archivePath = Paths.get(cfg.file.archivedStatsRepoPath)
    val archivedDataFiles = pathToList(archivePath).flatMap { archive =>
      pathToList(archive.resolve("data").resolve(cfg.general.world))
    }
    val missingDataPath = Paths.get(cfg.file.missingDataPath, cfg.general.world)
    val currentDataFiles = pathToList(path)
    val allDataFiles = currentDataFiles ::: archivedDataFiles
    val missingDataFiles = pathToList(missingDataPath).filterNot { p =>
      // Don't use mising data file if tibia kill stats file exists of the same date
      allDataFiles.map(_.getFileName()).contains(p.getFileName())
    }
    (allDataFiles ::: missingDataFiles)
      .filterNot(p => p.toFile.getName == "latest.json")
  }

}
