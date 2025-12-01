package org.jro.adventofcode.y2025

import org.jro.adventofcode
import org.jro.adventofcode.Error
import org.jro.adventofcode.InputData

/**
 * @author joe_mojo.
 *         2025/12/01
 */
object Day1 {
	enum Turn(val delta: Int) {
		case L extends Turn(-1)
		case R extends Turn(1)
	}
	case class WheelMove(turn: Turn, ticks: Int)
	object WheelMove {
		def unapply(s: String): Option[(Turn, Int)] = {
			s.headOption.flatMap {
				case 'L' => Some((Turn.L, s.tail.toInt))
				case 'R' => Some((Turn.R, s.tail.toInt))
				case _   => None
			}
		}
	}
	case class ZeroCounter(currentSelection: Int, value: Int)


	def inputParser(lines: InputData): Either[Error, Seq[WheelMove]] = {
		Right(lines.source.getLines().flatMap(WheelMove.unapply).map(WheelMove.apply).toSeq)
	}

	def puzzle1(input: Seq[WheelMove]): Int = {
		input.scanLeft(50){ (currentSelection, move) =>
			(currentSelection + move.turn.delta * move.ticks) % 100
		}.count(_ == 0)
	}

	def puzzle2(input: Seq[WheelMove]): Int = {
		val finalCounter = input.foldLeft(ZeroCounter(50, 0)){ (counter, move) =>
			val duringMoveZeros = ((100 + move.turn.delta * counter.currentSelection)%100 + move.ticks) / 100
			ZeroCounter(
				(counter.currentSelection + move.turn.delta * move.ticks) % 100,
				counter.value + duringMoveZeros
			)
		}
		finalCounter.value
	}

	def main(args: Array[String]): Unit = adventofcode.y2025.mainWithTransformer(1, puzzle1, puzzle2, inputParser)
	//OK:	1) 997	2) 5978
}
