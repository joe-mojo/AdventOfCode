package org.jro.adventofcode.y2023

import org.jro.adventofcode
import org.jro.adventofcode.{getInputLines, getInputLinesOrThrow}

import scala.annotation.tailrec

/**
 * @author joe_mojo.
 *         2023/12/03
 */
object Day3 {
	case class LocatedChar(char: Char, row: Int, col: Int)
	case class LocatedNumber(chars: Set[LocatedChar]){
		def toInt: Int = {
			String(chars.toSeq.sortBy(lc => lc.col).map(_.char).toArray).toInt
		}
	}

	def isSymbol(char: Char): Boolean = !(char.isDigit || char == '.')

	def  findDigitsAround(lines: IndexedSeq[String], row: Int, col: Int): Seq[LocatedChar] ={
		(for {
			cRow <- (row - 1) to (row + 1)
			cCol <- (col - 1) to (col + 1)
		} yield (cRow, cCol)).filter {
			case (`row`, `col`) => false
			case (r, c) if r < 0 || c < 0 || r > lines.length || c > lines.head.length => false
			case (r, c) if lines(r)(c).isDigit => true
			case _ => false
		}.map(loc => LocatedChar(lines(loc._1)(loc._2), loc._1, loc._2))
	}

	def findNumbersAround(lines: IndexedSeq[String], row: Int, col: Int): Set[Int] = {
		def completeNumber(dir: Int, currentRow: Int, currentCol: Int, line: String, locatedDigits: Set[LocatedChar]): Set[LocatedChar] = {
			if(currentCol >= 0 && currentCol < line.length) {
				if(line(currentCol).isDigit) {
					// FIXME on ne passe pas ici ???
					completeNumber(dir, currentRow, currentCol + dir, line,  locatedDigits + LocatedChar(line(currentCol), currentRow, currentCol))
				} else locatedDigits
			} else locatedDigits
		}
		val locatedNumbers: Set[LocatedNumber] = findDigitsAround(lines, row, col).map(lc =>
			LocatedNumber(completeNumber(1, lc.row, lc.col, lines(lc.row), Set.empty) ++
				completeNumber(-1, lc.row, lc.col, lines(lc.row), Set.empty))
		).toSet
		locatedNumbers.map(_.toInt)
	}

	def puzzle1(lines: IndexedSeq[String]): Int = {
		// Hypothèse : un nombre a au plus 1 seul symbol adjacent.
		// Corrolaire : pas de doublons de nombre
		(for {
			row <- lines.indices
			col <- 0 until lines(0).length
		} yield {
			if(isSymbol(lines(row)(col))){
				findNumbersAround(lines, row, col)
			} else Seq.empty[Int]
		}).flatten.sum
	}

	def main(args: Array[String]): Unit = {
		val lines: IndexedSeq[String] = getInputLinesOrThrow(2023, 3).toIndexedSeq
		println(puzzle1(lines)) // 498559
	}
}
