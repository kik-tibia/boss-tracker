package com.kiktibia.bosstracker.tracker.service

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
import scala.jdk.CollectionConverters.*

import sys.process.*

class FileIO(cfg: Config) extends CirceCodecs {

  def parseBossFile(): Either[Error, BossList] = {
    val jsonString: String = new String(Files.readAllBytes(Paths.get(cfg.file.bossListFileName)))
    parser.decode[BossList](jsonString)
  }

  def parseAllHistoricStats(bossList: BossList): List[KillStatsDay] = {
    getAllStatsFiles()
      .map(f => new String(Files.readAllBytes(f)))
      .map { s =>
        parser.decode[KillStatsDay](s).map(ksd => removeIrrelevantEntries(ksd, bossList))
      }
      .flatMap(_.toOption)
  }

  def getDateInfo(): Either[Error, DateInfo] = {
    val jsonString: String = new String(Files.readAllBytes(Paths.get(cfg.file.dateInfoFileName)))
    parser.decode[DateInfo](jsonString)
  }

  def updateBossStatsRepo() = {
    // Doing this instead of a git pull because sometimes the repo is force pushed to and diverges from the local branch
    Process(Seq("git", "fetch"), Some(new java.io.File(cfg.file.statsRepoPath))).!
    Process(Seq("git", "reset", "--hard", "origin/main"), Some(new java.io.File(cfg.file.statsRepoPath))).!
  }

  def getLastKilledUpdate(): LocalDate =
    LocalDate.parse(
      new String(Files.readAllBytes(Paths.get(cfg.file.statsDateFileName))).trim(),
      DateTimeFormatter.ISO_DATE
    )

  def getLastPredictionsUpdate(): LocalDate =
    LocalDate.parse(
      new String(Files.readAllBytes(Paths.get(cfg.file.predictionsDateFileName))).trim(),
      DateTimeFormatter.ISO_DATE
    )

  def updateDateFile(dateFile: String, date: LocalDate): Unit = {
    val path = Paths.get(dateFile)
    Files.write(path, date.toString().getBytes())
  }

  def parseLatestStats(): Either[Error, KillStatsDay] = {
    val path = getAllStatsFiles().sortBy(_.getFileName()).reverse.head
    val jsonString: String = new String(Files.readAllBytes(path))
    parser.decode[KillStatsDay](jsonString)
  }

  def getTimeOfLastMwcPost(): ZonedDateTime =
    ZonedDateTime.parse(
      new String(Files.readAllBytes(Paths.get(cfg.file.lastMwcPostFileName))).trim(),
      DateTimeFormatter.ISO_DATE_TIME
    )

  def getTimeOfLastMwcChange(): ZonedDateTime = {
    val mwcHistoryFiles = pathToList(Paths.get(cfg.file.mwcHistoryPath))
    ZonedDateTime.parse(
      mwcHistoryFiles.map(_.getFileName()).max.toString().split("\\.").head,
      DateTimeFormatter.ISO_DATE_TIME
    )
  }

  def getMwcDetails(): String = {
    val mwcHistoryFiles = pathToList(Paths.get(cfg.file.mwcHistoryPath))
    val latestFile = mwcHistoryFiles.maxBy(_.getFileName())
    new String(Files.readAllBytes(latestFile))
  }

  def updateLastMwcPost(date: ZonedDateTime): Unit = {
    val path = Paths.get(cfg.file.lastMwcPostFileName)
    Files.write(path, date.format(DateTimeFormatter.ISO_DATE_TIME).getBytes())
  }

  def parseRaidData(): String = {
    val path = Paths.get(cfg.file.raidDataFile)
    val jsonString: String = new String(Files.readAllBytes(path))
    jsonString
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

  private def getAllStatsFiles(): List[Path] = {
    val path = Paths.get(cfg.file.statsRepoPath, "data", cfg.general.world)
    val missingDataPath = Paths.get(cfg.file.missingDataPath, cfg.general.world)
    val dataFiles = pathToList(path)
    val missingDataFiles = pathToList(missingDataPath).filterNot { p =>
      // Don't use mising data file if tibia kill stats file exists of the same date
      dataFiles.map(_.getFileName()).contains(p.getFileName())
    }
    (dataFiles ::: missingDataFiles)
      .filterNot(p => p.toFile.getName == "latest.json")
  }

}
