package org.jro.adventofcode.y2020

import org.jro.adventofcode.{Error, getInputSourceOf}
import org.jro.adventofcode.Error.{UnknownEnumValue, sequence}
import org.jro.adventofcode.y2020.Day11.Status.{EmptySeat, Floor, OccupiedSeat}

import scala.annotation.tailrec
import scala.collection.{mutable => m}

object Day11 {
	sealed trait Status {
		def symbol: Char
	}
	object Status {
		case object Floor extends Status {
			override val symbol: Char = '.'
		}
		case object EmptySeat extends Status {
			override val symbol: Char = 'L'
		}
		case object OccupiedSeat extends Status {
			override val symbol: Char = '#'
		}

		def ofSymbol(s: Char): Option[Status] = {
			s match {
				case Floor.symbol => Some(Floor)
				case EmptySeat.symbol => Some(EmptySeat)
				case OccupiedSeat.symbol => Some(OccupiedSeat)
				case _ => None
			}
		}
	}

	case class SeatMap(seats: IndexedSeq[IndexedSeq[Status]]) {
		def statusAt(col: Int, row: Int): Status = seats(row)(col)

		def nextSeatMap: SeatMap = {
			SeatMap(
				for (row <- seats.indices) yield {
					for(col <- seats(row).indices) yield {
						nextStatus(col, row)
					}
				}
			)
		}

		def nextSeatMap2: SeatMap = {
			SeatMap(
				for (row <- seats.indices) yield {
					for(col <- seats(row).indices) yield {
						nextStatus2(col, row)
					}
				}
			)
		}

		def nextStatus(col: Int, row: Int): Status = {
			val occupiedCount = adjacentOccupiedStatusCount(col, row)
			statusAt(col, row) match {
				//If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
				case EmptySeat if occupiedCount == 0 => OccupiedSeat
				//If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
				case OccupiedSeat if occupiedCount >= 4 => EmptySeat
				case otherStatus => otherStatus
			}
		}

		def nextStatus2(col: Int, row: Int): Status = {
			val occupiedCount = visibleOccupiedStatusCount(col, row)
			statusAt(col, row) match {
				case EmptySeat if occupiedCount == 0 => OccupiedSeat
				case OccupiedSeat if occupiedCount >= 5 => EmptySeat
				case otherStatus => otherStatus
			}
		}

		def adjacentIndices(col: Int, row: Int): Seq[(Int, Int)] = {
			(for {
				currentRow <- (row - 1) to (row + 1)
				currentCol <- (col - 1) to (col + 1)
			} yield (currentCol, currentRow)).filter {
				case (c, r) =>
					isInMap(c, r) && !(c == col && r == row)
			}
		}
		def isInMap(col: Int, row: Int): Boolean = {
			col >= 0 && row >= 0 && row < seats.length && col < seats(row).length
		}

		def visibleIndicesFrom(col: Int, row: Int): Seq[(Int, Int)] = {
			(for {
				currentDx <- -1 to 1
				currentDy <- -1 to 1
			} yield (currentDx, currentDy)).filter {
				case (dx, dy) => !(dx == 0 && dy == 0)
			}.map { delta =>
				firstNonFloor(col, row, delta._1, delta._2)
			}.flatten
		}

		@tailrec
		final def firstNonFloor(col: Int, row: Int, dx: Int, dy: Int): Option[(Int, Int)] = {
			val currentRow = row + dy
			val currentCol = col + dx
			if(isInMap(currentCol, currentRow)) {
				if(statusAt(currentCol, currentRow) != Floor) Some((currentCol, currentRow))
				else firstNonFloor(currentCol, currentRow, dx, dy)
			} else None
		}


		def adjacentStatuses(col: Int, row: Int): Seq[Status] = {
			adjacentIndices(col, row).map {
				case (col, row) => statusAt(col, row)
			}
		}

		def visibleStatusesFrom(col: Int, row: Int): Seq[Status] = {
			visibleIndicesFrom(col, row).map {
				case (col, row) => statusAt(col, row)
			}
		}

		def adjacentOccupiedStatusCount(col: Int, row: Int): Int = { adjacentStatuses(col, row).count(_ == OccupiedSeat) }

		def visibleOccupiedStatusCount(col: Int, row: Int): Int = { visibleStatusesFrom(col, row).count(_ == OccupiedSeat)}

		def show: String = {
			seats.map { row =>
				row.map(_.symbol).mkString("")
			}.mkString("\n")
		}
	}

	object SeatMap {
		def ofLines(input: Iterator[String]): Either[Error, SeatMap] = {
			Error.sequence(input.map { line =>
				Error.sequence(line.map { c =>
					Status.ofSymbol(c).fold[Either[Error, Status]](Left(UnknownEnumValue[Status](s"$c")))(Right(_))
				})
			}).map(rows => SeatMap(rows.toIndexedSeq))
		}
	}

	case class RoundIterator(seatMap: SeatMap) {
		private val last2: m.Queue[SeatMap] = m.Queue(seatMap, seatMap.nextSeatMap)
		private var round = 1

		def stabilized: Boolean = last2.head == last2.last
		def lastRound: Int = round
		def last2Map: (SeatMap, SeatMap) = (last2.head, last2.last)

		def playNextRound(): Boolean = {
			if(stabilized) true
			else {
				last2.dequeue()
				last2 += last2.head.nextSeatMap
				round += 1
				stabilized
			}
		}

		def playNextRound2(): Boolean = {
			if(stabilized) true
			else {
				last2.dequeue()
				last2 += last2.head.nextSeatMap2
				round += 1
				stabilized
			}
		}

		def playUntilStabilized(): SeatMap = {
			while (!playNextRound())()
			last2.last
		}

		def playUntilStabilized2(): SeatMap = {
			while (!playNextRound2())()
			last2.last
		}

	}

	def main(args: Array[String]): Unit = {
		val maybeSeatMap: Either[Error, SeatMap] = getInputSourceOf(2020, 11).map(_.getLines()).flatMap(SeatMap.ofLines)

		//println(s"seatMap = ${maybeSeatMap.map(_.show)}\n======================================\n")

		val res1 = maybeSeatMap.map { seatMap =>
			val finalMap = RoundIterator(seatMap).playUntilStabilized()
			println(finalMap.show)
			finalMap.seats.map {row =>
				row.count(_ == OccupiedSeat)
			}.sum
		}
		println(s"\nres1 = $res1")

		val res2 = maybeSeatMap.map { seatMap =>
			val finalMap = RoundIterator(seatMap).playUntilStabilized2()
			println(finalMap.show)
			finalMap.seats.map {row =>
				row.count(_ == OccupiedSeat)
			}.sum
		}
		println(s"\nres2 = $res2")
	}
}
