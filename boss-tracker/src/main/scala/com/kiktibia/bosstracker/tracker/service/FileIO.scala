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
import java.nio.file.Paths
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters.*

import sys.process.*
import java.nio.file.Path

class FileIO(cfg: Config) extends CirceCodecs {

  def parseBossFile(): Either[Error, BossList] = {
    val jsonString: String = new String(Files.readAllBytes(Paths.get(cfg.file.bossListFileName)))
    parser.decode[BossList](jsonString)
  }

  def parseAllHistoricStats(): List[KillStatsDay] = {
    val files = getAllStatsFiles()

    files.map(f => new String(Files.readAllBytes(f))).map(parser.decode[KillStatsDay]).flatMap(_.toOption)
  }

  def getDateInfo(): Either[Error, DateInfo] = {
    val jsonString: String = new String(Files.readAllBytes(Paths.get(cfg.file.dateInfoFileName)))
    parser.decode[DateInfo](jsonString)
  }

  def updateBossStatsRepo() = {
    Process(s"git pull", Some(new java.io.File(cfg.file.statsRepoPath))).!
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
    (pathToList(path) ::: pathToList(missingDataPath))
      .filterNot(p => p.toFile.getName == "latest.json")
  }

}
