package org.jro.adventofcode.y2020

import org.jro.adventofcode
import org.jro.adventofcode.Error.Empty

import scala.annotation.tailrec

object Day3 extends App {
	val slopes: Seq[(Int, Int)] = Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))

	case class Point(x: Int, y: Int) {
		def move(dx: Int, dy: Int): Point = {
			Point(x + dx, y + dy)
		}
	}
	object Point {
		val Origin: Point = Point(0, 0)
	}

	case class SlopeMap(treeLocations: Set[Point], patternWidth: Int = 31) {
		val ymax: Int = treeLocations.map(_.y).max

		def isTree(x: Int, y: Int): Boolean = {
			treeLocations.contains(Point(x % patternWidth, y))
		}

		@inline
		def isTree(location: Point): Boolean = {
			isTree(location.x, location.y)
		}
	}

	object SlopeMap {
		val TreeChar = '#'
		val OpenChar = '.'

		def collectTreeLocations(lineTxt: String, lineIdx: Int): Set[Point] = {
			lineTxt.zipWithIndex.foldLeft(Set.empty[Point]){ (lineSet, charWithIndex) =>
				charWithIndex match {
					case (`TreeChar`, colIdx) => lineSet + Point(x = colIdx, y = lineIdx)
					case  _ => lineSet
				}
			}
		}

		def fromLines(lines: Iterator[String], lineLength: Int): SlopeMap = {
			SlopeMap(lines.zipWithIndex.foldLeft(Set.empty[Point]){ (fullSet, lineWithIndex) =>
				val (lineTxt, lineIdx) = lineWithIndex
				fullSet ++ collectTreeLocations(lineTxt, lineIdx)
			}, lineLength)
		}
	}

	@tailrec
	def countTrees(map: SlopeMap, currentLocation: Point = Point.Origin, currentCount: Int = 0, slope: (Int, Int)): Int = {
		if(currentLocation.y > map.ymax) {
			currentCount
		} else {
			countTrees(
				map,
				currentLocation.move(slope._1, slope._2),
				if(map.isTree(currentLocation)) currentCount + 1 else currentCount,
				slope
			)
		}
	}

	def puzzle1(lines: Iterator[String], lineLength: Int): Int = {
		countTrees(SlopeMap.fromLines(lines, lineLength), slope = (3, 1))
	}

	def puzzle2(lines: Iterator[String], lineLength: Int): Long = {
		val map = SlopeMap.fromLines(lines, lineLength)
		(for(slope <- slopes) yield countTrees(map, slope = slope)).map(_.toLong).product
	}

	val maybeLineLength = adventofcode.getInputData(2020, 3).flatMap { inputData =>
		val lines = inputData.source.getLines()
		val res = if(lines.hasNext) Right(lines.next().length) else Left(Empty(inputData.resource))
		inputData.source.close()
		res
	}

	val res1 = 	maybeLineLength.flatMap { lineLength =>
		adventofcode.getInputSourceOf(2020, 3).map (source => puzzle1(source.getLines(), lineLength))
	}

	val res2 = maybeLineLength.flatMap { lineLength =>
		adventofcode.getInputSourceOf(2020, 3).map (source => puzzle2(source.getLines(), lineLength))
	}

	println(s"res1 = ${res1}\nres2 = $res2")
}
