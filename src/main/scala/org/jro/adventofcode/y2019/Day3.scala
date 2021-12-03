package org.jro.adventofcode.y2019

import org.jro.adventofcode.findResourceOpt

import scala.io.Source

case class Point2(x: Int, y: Int) {
	def +(v: Vector2): Point2 = {
		Point2(x + v.dx, y + v.dy)
	}

	def anchor(v: Vector2): AnchoredVector2 = {
		AnchoredVector2(this, v)
	}
}

case class Vector2(dx: Int, dy: Int) {
	def anchorAt(p: Point2): AnchoredVector2 = {
		AnchoredVector2(p, this)
	}
}

case class AnchoredVector2(p1: Point2, vector: Vector2) {
	val p2: Point2 = p1 + vector
	def length: Int = Day3.manathanDistance(p1)(p2)
	def raster: Seq[Point2] = {
		for{
			xi <- p1.x to p2.x by (if(vector.dx >= 0) 1 else -1)
			yi <- p1.y to p2.y by (if(vector.dy >= 0) 1 else -1)
		} yield Point2(xi, yi)
	}
}


object Day3 {
	type Path = Seq[Point2]
	type Wire = Seq[AnchoredVector2]
	val vecRegx = """([URDL])(\d+)""".r
	val origin = Point2(0, 0)

	def main(args: Array[String]): Unit = {
		val display = findResourceOpt("2019/day3/input").map {
			Source.fromURL(_)
		}.map { src =>
			src.getLines()
		}.map { lineIter =>
			val wires: Seq[Wire] = lineIter.map(parseLine).toSeq
			findCrossingPoints(wires.head, wires.tail.head).sortBy(manathanDistance(origin))
		}.map { crossingPoints =>
			println(s"Crossing points: $crossingPoints")
			crossingPoints.head
		}.map(_.toString).getOrElse("N/A")


		println(s"Result for day3-1: $display") //Point2(-610,675) => 1285

		val display2 = findResourceOpt("2019/day3/input").map {
			Source.fromURL(_)
		}.map { src =>
			src.getLines()
		}.map { lineIter =>
			val wires: Seq[Wire] = lineIter.map(parseLine).toSeq
			findCrossingPoints(wires.head, wires.tail.head).sortBy(manathanDistance(origin))
					.map(stepToIntersection(wires.head, wires.tail.head)).sorted
		}.map {crossingPointsSteps =>
			println(s"Crossing points: $crossingPointsSteps")
			crossingPointsSteps.head
		}.map(_.toString).getOrElse("N/A")

		println(s"Result for day3-2: $display2") //14228
	}

	def parseLine(line: String): Seq[AnchoredVector2] = {
		line.split(",").map {
			case vecRegx(dirString, normString) => (dirString, normString.toInt)
		}.map {
			case ("U", norm) => Vector2(0, norm)
			case ("R", norm) => Vector2(norm, 0)
			case ("D", norm) => Vector2(0, -norm)
			case ("L", norm) => Vector2(-norm, 0)
		}.foldLeft(Seq.empty[AnchoredVector2]) { (avecs, vec) =>
			if(avecs.isEmpty) Seq(Point2(0, 0) anchor vec)
			else avecs :+ (avecs.last.p2 anchor vec)
		}
	}

	def stepToIntersection(w1: Wire, w2: Wire): Point2 => Option[Int] = stepsToIntersection(rasterWire(w1), rasterWire(w2))

	def stepsToIntersection(rasterW1: Seq[Point2], rasterW2: Seq[Point2])(intersectionPoint: Point2): Option[Int] = {
		for{
			i1 <- Option(rasterW1.indexOf(intersectionPoint)) if i1 >= 0
			i2 <- Option(rasterW2.indexOf(intersectionPoint)) if i2 >= 0
		} yield i1 + i2
	}

	def rasterWire(w: Wire): Seq[Point2] = {
		w.tail.foldLeft(w.head.raster) { (wPoints, aVec) =>
			wPoints ++ aVec.raster.tail
		}
	}

	def findCrossingPoints(w1: Wire, w2: Wire): Seq[Point2] = {
		rasterWire(w1).tail.toSet.intersect(rasterWire(w2).tail.toSet).toSeq
	}

	def dist(coord1: Int, coord2: Int): Int = Math.abs(coord1 - coord2)

	def manathanDistance(p1: Point2)(p2: Point2): Int = {
		Math.abs(p2.x - p1.x) + Math.abs(p2.y - p1.y)
	}

}
