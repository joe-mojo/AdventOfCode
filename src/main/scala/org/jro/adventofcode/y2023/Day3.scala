package org.jro.adventofcode.y2023

import org.jro.adventofcode
import org.jro.adventofcode.{getInputLines, getInputLinesOrThrow}

/**
 * @author joe_mojo.
 *         2023/12/03
 */
object Day3 {
	case class LocatedChar(char: Char, row: Int, col: Int)
	case class LocatedNumber(chars: Seq[LocatedChar]){
		def toInt: Int = {
			chars.sortBy(lc => lc.col).map(_.char).mkString.toInt
		}
	}

	def isSymbol(char: Char): Boolean = !(char.isDigit || char == '.')

	def  findDigitsAround(lines: IndexedSeq[String], row: Int, col: Int): Seq[LocatedChar] ={
		(for {
			cRow <- (row - 1) until (row + 1)
			cCol <- (col - 1) until (col + 1)
		} yield (cRow, cCol)).filter {
			case (`row`, `col`) => false
			case (r, c) if r < 0 || c < 0 || r > lines.length || c > lines.head.length => false
			case (r, c) if lines(r)(c).isDigit => true
			case _ => false
		}.map(loc => LocatedChar(lines(loc._1)(loc._2), loc._1, loc._2))
	}

	def findNumbersAround(lines: IndexedSeq[String], row: Int, col: Int): Seq[Int] = {
		def findNumberDigitsAround(row: Int, col: Int, lines: IndexedSeq[String], locatedDigits: Seq[LocatedChar], result: Set[LocatedNumber]): Int = {
			0 //TODO
		}
		val locatedDigits = findDigitsAround(lines, row, col)
		//TODO
	}

	def puzzle1(lines: IndexedSeq[String]): Int = {
		// Hypothèse : un nombre a au plus 1 seul symbol adjacent.
		// Corrolaire : pas de doublons de nombre
		(for {
			row <- 0 to lines.length
			col <- 0 to lines(0).length
		} yield {
			if(isSymbol(lines(row)(col))){
				findNumbersAround(lines, row, col)
			} else Seq.empty[Int]
		}).flatten.sum

	}
}
	def main(args: Array[String]): Unit = {
		val lines = getInputLinesOrThrow(2023, 3).toIndexedSeq
}
