package org.jro.adventofcode.y2021

import org.jro.adventofcode

import scala.collection.{mutable => m}
import java.lang.Math.{abs, min, max}


case class Point(x: Int, y: Int)
case class Vect(dx: Int, dy: Int) {
	def u: Vect = {
		Vect(dx / max(1, abs(dx)), dy / max(1, abs(dy)))
	}
}
object Vect {
	def of(p1: Point, p2: Point): Vect = {
		Vect(p2.x - p1.x, p2.y - p1.y)
	}
}

/**
 * @author joe_mojo.
 *         2021/12/05
 */
object Day5 {
	//405,945 -> 780,945
	val SegmentRegex = """(\d+),(\d+) -> (\d+),(\d+)""".r

	def parseLine(line: String): (Point, Point) = {
		line match {
			case SegmentRegex(x1, y1, x2, y2) => Point(x1.toInt, y1.toInt) -> Point(x2.toInt, y2.toInt)
		}
	}

	def expandHV(segment: (Point, Point)): Seq[Point] = {
		if(segment._1.x == segment._2.x) {
			if(segment._1.y > segment._2.y) expandHV((segment._2, segment._1))
			else (segment._1.y to segment._2.y).map(y => Point(segment._1.x, y))
		} else if(segment._1.y == segment._2.y) {
			if(segment._1.x > segment._2.x) expandHV((segment._2, segment._1))
			else (segment._1.x to segment._2.x).map(x => Point(x, segment._1.y))
		} else Seq.empty
	}

	def expandHVD(segment: (Point, Point)): Seq[Point] = {
		val u: Vect = Vect.of(segment._1, segment._2).u
		val xrange = segment._1.x to (segment._2.x, if(u.dx == 0) 1 else u.dx)
		val yrange = segment._1.y to (segment._2.y, if(u.dy ==0) 1 else u.dy)
		xrange.zipAll(yrange, segment._1.x, segment._1.y).map(coords => Point(coords._1, coords._2))
	}

	def storePoints(pointMap: m.Map[Point, Int], points: Seq[Point]): m.Map[Point, Int] = {
		points.foldLeft(pointMap){ (map, point) =>
			map.put(point, map.get(point).map(_ + 1).getOrElse(1))
			map
		}
	}

	val inputSegments: Either[adventofcode.Error, IndexedSeq[(Point, Point)]] = adventofcode.getInputData(2021, 5).map { inputData =>
		inputData.source.getLines().map(parseLine).toIndexedSeq
	}

	def puzzleOne(inputSegments: Either[adventofcode.Error, IndexedSeq[(Point, Point)]], expandFn: ((Point, Point)) => Seq[Point]): Either[adventofcode.Error, Int] = {
		inputSegments.map { segments =>
			val pointMap: Map[Point, Int] = segments.map(expandFn).foldLeft(m.Map.empty[Point, Int]){ (map, points) =>
				storePoints(map, points)
			}.toMap
			val dangerousPointMap = pointMap.collect {
				case entry@(point, i) if i > 1 => entry
			}
			//println(s"Dangerous point map: ${dangerousPointMap}")
			dangerousPointMap.size
		}
	}

	def main(args: Array[String]): Unit = {
		println(s"Day5.1 : ${puzzleOne(inputSegments, expandHV)}")
		println(s"Day5.2 : ${puzzleOne(inputSegments, expandHVD)}")
	}

}
