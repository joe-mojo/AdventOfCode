package org.jro.adventofcode.y2024

import org.jro.adventofcode

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.matching.Regex
/**
 * @author joe_mojo.
 *         2024/12/10
 */
object Day4 {
	private val XmasRegex = """XMAS""".r
	private val XmasLength = 4
	private val SamxRegex = """SAMX""".r
	private val SamxLength = 4


	case class SquarePattern(pattern: Regex, side: Int)
	private val Xmas2dPatterns = IndexedSeq(
		SquarePattern("""M.S.A.M.S""".r, 3),
		SquarePattern("""M.M.A.S.S""".r, 3),
		SquarePattern("""S.M.A.S.M""".r, 3),
		SquarePattern("""S.S.A.M.M""".r, 3)
	)

	case class Text(value: IndexedSeq[String]) {
		def flattenSquareArea(row: Int, col: Int, side: Int): String = {
			value.slice(row, row + side).map(_.slice(col, col + side)).mkString
		}

		def countMatches(pattern: SquarePattern): Int = {
			val numRows = value.length
			val numCols = value.head.length
			(0 until numRows).flatMap { row =>
				(0 until numCols).map { col =>
					flattenSquareArea(row, col, pattern.side)
				}
			}.count(pattern.pattern.matches)
		}
	}

	private def countAllMatches(lines: Seq[String], pattern: Regex): Int = {
		lines.map { line =>
			pattern.findAllMatchIn(line).size
		}.sum
	}

	private def transpose(strings: IndexedSeq[String]): IndexedSeq[String] = {
		if (strings.isEmpty) IndexedSeq.empty
		else {
			val length = strings.head.length
			(0 until length).map { i =>
				strings.map(_(i)).mkString
			}
		}
	}

	def getAllDiagonals(strings: IndexedSeq[String])(implicit ec: ExecutionContext): Future[IndexedSeq[String]] = {
		if (strings.isEmpty) Future.successful(IndexedSeq.empty)
		else {
			val numRows = strings.length
			val numCols = strings.head.length

			def extractDiagonal(startRow: Int, startCol: Int, rowInc: Int, colInc: Int): String = {
				Iterator.iterate((startRow, startCol)) { case (row, col) => (row + rowInc, col + colInc) }
						.takeWhile { case (row, col) => row >= 0 && row < numRows && col >= 0 && col < numCols }
						.map { case (row, col) => strings(row)(col) }
						.mkString
			}

			val eventualTopLeftToBottomRight = Future {
				(for {
					row <- 0 until numRows
					col <- 0 until numCols
					if row == 0 || col == 0
				} yield extractDiagonal(row, col, 1, 1)).filter(diag => diag.length >= Math.min(XmasLength, SamxLength))
			}

			val eventualTopRightToBottomLeft = Future{
				(for {
					row <- 0 until numRows
					col <- 0 until numCols
					if row == 0 || col == numCols - 1
				} yield extractDiagonal(row, col, 1, -1)).filter(diag => diag.length >= Math.min(XmasLength, SamxLength))
			}

			for {
				topLeftToBottomRight <- eventualTopLeftToBottomRight
				topRightToBottomLeft <- eventualTopRightToBottomLeft
			} yield (topLeftToBottomRight ++ topRightToBottomLeft)
		}
	}




	def puzzle1(input: Iterator[String])(implicit ec: ExecutionContext): Int = {
		val lines = input.toIndexedSeq
		val eventualHorizontalCount = Future(countAllMatches(lines, XmasRegex))
		val evenualHorizontalReverseCount = Future(countAllMatches(lines, SamxRegex))
		val eventualVerticalTotalCount = Future(transpose(lines)).flatMap { columns =>
			val eventualVerticalCount = Future(countAllMatches(columns, XmasRegex))
			val evenualVerticalReverseCount = Future(countAllMatches(columns, SamxRegex))
			for {
				verticalCount <- eventualVerticalCount
				verticalReverseCount <- evenualVerticalReverseCount
			} yield verticalCount + verticalReverseCount
		}
		val eventualDiagonalsTotalCount = getAllDiagonals(lines).flatMap { diagonals =>
			val eventualDiagonalCount = Future(countAllMatches(diagonals, XmasRegex))
			val evenualDiagonalReverseCount = Future(countAllMatches(diagonals, SamxRegex))
			for {
				diagonalCount <- eventualDiagonalCount
				diagonalReverseCount <- evenualDiagonalReverseCount
			} yield diagonalCount + diagonalReverseCount
		}
		val eventualGrandTotal: Future[Int] = (for {
			horizontalCount <- eventualHorizontalCount
			horizontalReverseCount <- evenualHorizontalReverseCount
			verticalTotalCount <- eventualVerticalTotalCount
			diagonalsTotalCount <- eventualDiagonalsTotalCount
		} yield horizontalCount + horizontalReverseCount + verticalTotalCount + diagonalsTotalCount)

		Await.result(eventualGrandTotal, scala.concurrent.duration.Duration.Inf)
	}

	def puzzle2(input: Iterator[String]): Int = {
		val text = Text(input.toIndexedSeq)
		Xmas2dPatterns.map(text.countMatches).sum
	}

	def main(args: Array[String]): Unit = {
		implicit val ec: ExecutionContext = ExecutionContext.global
		adventofcode.y2024.classicMain[Int](4, puzzle1, puzzle2)
	}
}
