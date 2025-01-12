package org.jro.adventofcode.y2024

import org.jro.adventofcode
import org.jro.adventofcode.Error.{Errors, IOError, InputError, NaN, sequence}
import org.jro.adventofcode.InputData
import org.jro.adventofcode.y2024.mainWithTransformer

import scala.collection.mutable.Map as MutableMap
import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.concurrent.duration.*
import scala.util.Try
import scala.jdk.CollectionConverters.*

/**
 * @author joe_mojo.
 *         2025/01/11
 */
object Day10 {

	case class Coord(x: Int, y: Int)

	case class TopographicMap(heighs: IndexedSeq[IndexedSeq[Int]]) {
		val sideLength = heighs.size

		def apply(x: Int, y: Int): Int = heighs(y)(x)
		def apply(coord: Coord): Int = heighs(coord.y)(coord.x)

		def isDefinedPoint(x: Int, y: Int): Boolean = x >= 0 && x < sideLength && y >= 0 && y < sideLength
		def isDefinedPoint(coord: Coord): Boolean = isDefinedPoint(coord.x, coord.y)

		def next(x: Int, y: Int): List[Coord] = {
			List(Coord(x, y - 1), Coord(x, y + 1), Coord(x - 1, y), Coord(x + 1, y)).filter { coord =>
				isDefinedPoint(coord) && this(coord) == this(x, y) + 1
			}
		}
		def next(from: Coord): List[Coord] = next(from.x, from.y)

		def findStartingPoints: Seq[Coord] = {
			(0 until sideLength).flatMap { y =>
				(0 until sideLength).filter { x =>
					this(x, y) == 0
				}.map { x =>
					Coord(x, y)
				}
			}
		}

		def countAccessibleSummitsFrom(start: Coord): Int = {
			@tailrec
			def hike(visited: Set[Coord], toVisit: List[Coord]): Set[Coord] = {
				toVisit match {
					case Nil => visited
					case head :: tail =>
						val nextPoints = next(head).filterNot(c => visited.contains(c) || toVisit.contains(c))
						hike(visited + head, tail ++ nextPoints)
				}
			}

			hike(Set.empty[Coord], List(start)).count(coord => this (coord) == 9)
		}

		def countRatedPathsFrom(start: Coord): Int = {
			val rated = MutableMap.empty[Coord, Int]
			def hike(from: Coord): Int = {
				val nextPoints = next(from)
				nextPoints match
					case Nil if this(from) == 9 => 1
					case Nil => 0
					case _ =>
						nextPoints.map(c => rated.getOrElseUpdate(c, hike(c))).sum
			}
			hike(start)
		}
	}


	def parseLine(line: String): Either[Errors, IndexedSeq[Int]] = sequence[Int](line.map((c: Char) => Try(c.asDigit).toEither.left.map(_ => NaN(c.toString))).toIndexedSeq)
	def parseLines(lines: Iterator[String]): Either[Errors, TopographicMap] = {
		lines.foldLeft[Either[Errors, IndexedSeq[IndexedSeq[Int]]]](Right(IndexedSeq.empty[IndexedSeq[Int]])) { (memo, line) =>
			(memo, parseLine(line)) match {
				case (Right(resultSeq), Right(ints)) => Right(resultSeq :+ ints)
				case (Left(errors), Right(_)) => Left(errors)
				case (Right(_), Left(errors)) => Left(errors)
				case (Left(errors1), Left(errors2)) => Left(errors1 and errors2)
			}
		}.map(TopographicMap(_))
	}
	def parseInput(input: InputData): Either[Errors, TopographicMap] = {
		Try(input.source.getLines()).toEither.left.map(IOError(input.resource, _)).left.map(Errors(_)).flatMap(parseLines)
	}

	def countSummitsFrom(inputMap: TopographicMap, startingPoints: Seq[Coord]): Int = {
		val startTime = System.nanoTime()
		val res = startingPoints.map(inputMap.countAccessibleSummitsFrom).sum
		val durationNanos = System.nanoTime() - startTime
		println(s"countSummitsFrom duration for ${startingPoints.size} starting points: ${durationNanos.toDouble / 1_000_000} ms")
		res
	}

	def rateHikingTrails(inputMap: TopographicMap, startingPoints: Seq[Coord]): Int = {
		val startTime = System.nanoTime()
		val res = startingPoints.map(inputMap.countRatedPathsFrom).sum
		val durationNanos = System.nanoTime() - startTime
		println(s"rateHikingTrails duration for ${startingPoints.size} starting points: ${durationNanos.toDouble / 1_000_000} ms")
		res
	}

	def puzzle1(inputMap: TopographicMap)(implicit runtimeInfo: (ExecutionContext, Int)): Int = {
		val staringPoints = inputMap.findStartingPoints
		val partitionCount = runtimeInfo._2
		implicit val ec: ExecutionContext = runtimeInfo._1
		val partitionSize = staringPoints.size / partitionCount
		val futurSum = Future.sequence(
			for {
				coordsPartition <- staringPoints.grouped(partitionSize)
			} yield {
				Future(countSummitsFrom(inputMap, coordsPartition))
			}
		).map(_.sum)
		Await.result(futurSum, atMost = 10.minutes)
	}

	def puzzle2(inputMap: TopographicMap)(implicit runtimeInfo: (ExecutionContext, Int)): Int = {
		val staringPoints = inputMap.findStartingPoints
		val partitionCount = runtimeInfo._2
		implicit val ec: ExecutionContext = runtimeInfo._1
		val partitionSize = staringPoints.size / partitionCount
		val futurSum = Future.sequence(
			for {
				coordsPartition <- staringPoints.grouped(partitionSize)
			} yield {
				Future(rateHikingTrails(inputMap, coordsPartition))
			}
		).map(_.sum)
		Await.result(futurSum, atMost = 10.minutes)
	}

	def main(args: Array[String]): Unit = {
		// Parallelism is overkill for this puzzle,
		val threadCount = Runtime.getRuntime.availableProcessors
		val executorService = Executors.newFixedThreadPool(threadCount)
		implicit val runtimeInfo: (ExecutionContextExecutor, Int) = ExecutionContext.fromExecutor(executorService) -> threadCount
		try{
			mainWithTransformer(10, puzzle1(_) /* 794 OK */, puzzle2(_) /* 1706 OK */, parseInput)
		} finally {
			executorService.shutdown()
		}
	}
}
