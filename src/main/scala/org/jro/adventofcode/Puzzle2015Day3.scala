package org.jro.adventofcode

import scala.io.{Codec, Source}

sealed trait Direction {
	def symbol: Char
}
//v><^
case object North extends Direction {
	override val symbol = '^'
}
case object East extends Direction {
	override val symbol = '>'
}
case object South extends Direction {
	override val symbol = 'v'
}
case object West extends Direction {
	override val symbol = '<'
}

object Direction {
	def fromChar(d: Char): Option[Direction] = d match {
		case North.symbol => Some(North)
		case East.symbol => Some(East)
		case South.symbol => Some(South)
		case West.symbol => Some(West)
		case _ => None
	}
}

case class Coord(x: Long, y: Long) {
	def to(d: Direction): Coord = d match {
		case North => this.copy(y = this.y + 1 )
		case East => this.copy(x = this.x + 1 )
		case South => this.copy(y = this.y - 1)
		case West => this.copy(x = this.x - 1)
	}
}

case class State(currentPos: Coord, visited: Set[Coord]) {
	def move(dir: Direction): State = {
		val newPos = currentPos.to(dir)
		State(newPos, this.visited + newPos)
	}
}

object Puzzle2015Day3 extends App {

	def partOne() = {
		val initialState = State(Coord(0,0), Set(Coord(0,0)))
		implicit val codec = Codec("UTF-8")

		val finalState = Source.fromResource("adventofcode2015_3_input").iter.foldLeft(initialState) { (state, c) =>
			Direction.fromChar(c).map(state.move).getOrElse(state)
		}
		println(s"Visited ${finalState.visited.size} houses.")
	}

	partOne()
}
