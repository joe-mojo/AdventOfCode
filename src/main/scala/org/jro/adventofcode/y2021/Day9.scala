package org.jro.adventofcode.y2021

import org.jro.adventofcode
import org.jro.adventofcode.Error

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.concurrent.duration.*
import scala.collection.mutable.Set as MSet

object Day9 {
	implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(12))
	case class Point(row: Int, col: Int, heigt: Int)
	case class Basin(lowest: Point, allPoints: Set[Point]){
		def size: Int = allPoints.size
		def toSizeString = s"Basin(lowest=$lowest, size=${allPoints.size})"
	}

	def showDateTime: String = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)

	val errorOrHeightMap: Either[Error, IndexedSeq[IndexedSeq[Int]]] = adventofcode.getInputLines(2021, 9).map { lines =>
		lines.map{ line =>
			line.map(c => s"$c".toInt).toIndexedSeq
		}.toIndexedSeq
	}

	def listAdjacents(map: IndexedSeq[IndexedSeq[Int]], row: Int, col: Int): Seq[Point] = {
		Seq((row - 1, col), (row, col + 1), (row + 1, col), (row, col - 1)).filter {
			case (r, c) if r >= 0 && c >=0 && r < map.length && c < map.head.length => true
			case _ => false
		}.map {
			case (r, c) => Point(r, c, map(r)(c))
		}
	}

	def isLowest(map: IndexedSeq[IndexedSeq[Int]], row: Int, col: Int): Boolean = {
		listAdjacents(map, row, col).forall(pt => pt.heigt > map(row)(col))
	}

	def risk(value: Int): Int = 1 + value

	def collectLowests(map: IndexedSeq[IndexedSeq[Int]]): Seq[Point] = {
		(for {
			row <- 0 until map.length
			col <- 0 until map.head.length
		} yield (row, col, isLowest(map, row, col))).filter(_._3).map {
			case (row, col, _) => Point(row, col, map(row)(col))
		}
	}

	def totalRisk(lowests: Seq[Int]): Int = {
		lowests.map(risk).sum
	}

	def puzzle(map: IndexedSeq[IndexedSeq[Int]]): Int = {
		totalRisk(collectLowests(map).map(_.heigt))
	}

	def findBasinMut(lowest: Point, map: IndexedSeq[IndexedSeq[Int]]): Basin = {
		val basinPoints = MSet(lowest)
		def collectPoints(currentLoc: Point): Unit = {
			listAdjacents(map, currentLoc.row, currentLoc.col).foreach { adjPt =>
				if(adjPt.heigt < 9 && !basinPoints.contains(adjPt)) {
					basinPoints += adjPt
					collectPoints(adjPt)
				}
			}
		}
		collectPoints(lowest)
		Basin(lowest, basinPoints.toSet)
	}

	def findBasins(lowests: Seq[Point], map: IndexedSeq[IndexedSeq[Int]]): Future[Seq[Basin]] = {
		println(s"${lowests.length} lowest points.")
		Future.sequence(lowests.map { lowest =>
			Future(findBasinMut(lowest, map))
		})
	}

	def puzzle2(map: IndexedSeq[IndexedSeq[Int]]): Future[Long] = {
		findBasins(collectLowests(map), map).map { basins =>
			println(basins.map(_.toSizeString))
			basins.sortBy(b => -b.size).take(3).foldLeft(1L){ (total, value) =>
				total * value.size
			}
		}
	}

	def main(args: Array[String]): Unit = {
		println(s"Day9.1: ${errorOrHeightMap.map(puzzle)}")
		val sample: IndexedSeq[IndexedSeq[Int]] = """2199943210
												   |3987894921
												   |9856789892
												   |8767896789
												   |9899965678""".stripMargin
			.split("\\n").map{ line => line.map(c => s"$c".toInt)}.toIndexedSeq
		println(s"Sample: ${Await.result(puzzle2(sample), atMost = 1.minute)}")
		println(s"Day9.2: ${errorOrHeightMap.map( ptMap =>Await.result(puzzle2(ptMap), atMost = 1.hour))}")

	}

}
