package org.jro.adventofcode.y2021

import org.jro.adventofcode.getInputSourceOf

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Day4 extends App {

	case class Board(numbers: IndexedSeq[IndexedSeq[Int]]) {
		val lineLength = numbers.head.length
		val lineCount = numbers.length
		val marks: IndexedSeq[Array[Boolean]] = IndexedSeq.fill(numbers.length)(Array.fill(numbers.head.length)(false))

		def sumOfUnmarked: Int = {
			(for {
				line <- 0 until lineCount
				col <- 0 until lineLength
			} yield {
				if(! marks(line)(col)) numbers(line)(col)
				else 0
			}).sum
		}

		def mark(number: Int): Board = {
			@tailrec
			def tryMark(line: Int, col: Int): Unit = {
				if(numbers(line)(col) == number) marks(line)(col) = true // TODO check and mark board for won here to simplify check complexity afterward
				else if(col < lineLength - 1) tryMark(line, col + 1)
				else if(line < lineCount - 1) tryMark(line + 1, 0)
			}
			tryMark(0, 0)
			this
		}

		def hasMarkedLine: Boolean = {
			marks.find { markLine =>
				markLine.forall(b => b)
			}.nonEmpty
		}

		def hasMarkedCol: Boolean = {
			(for {
				col <- 0 until lineLength
			} yield {
				for {
					line <- 0 until lineCount
				} yield marks(line)(col)
			}).find { markCol =>
				markCol.forall(b => b)
			}.nonEmpty
		}

		def hasWon: Boolean = {
			hasMarkedLine || hasMarkedCol
		}

		override def toString: String = {
			(for {
				line <- 0 until lineCount
			} yield {
				(for {
					col <- 0 until lineLength
				} yield {
					if(marks(line)(col))
						s"\u001B[92m${numbers(line)(col)}${Console.RESET}"
					else
						s"${numbers(line)(col)}"
				}).mkString(" ")
			}).mkString("\n")
		}
	}

	def getRndSeqAndBoards(lines: Iterator[String]): (IndexedSeq[Int], Seq[Board]) = {
		(
			lines.next().split(",").map(_.toInt).toIndexedSeq,
			lines.sliding(6, 6).foldLeft(Seq.empty[Board]){ (boards, boardLines) =>
				Board(boardLines.tail.map(line => line.split(" ").filter(_.nonEmpty).map(_.toInt)).map(_.toIndexedSeq).toIndexedSeq) +: boards
			}
		)
	}

	def round(boards: Seq[Board], drawnNumber: Int): Option[Board] = {
		boards.foreach(_.mark(drawnNumber))
		boards.find(_.hasWon)
	}

	def puzzleOneCore(rndSeq: IndexedSeq[Int], boards: Seq[Board]): Option[(Int, Board, Int)] = {
		val maybeWinnerTrigger = rndSeq.find { rndNum =>
			round(boards, rndNum).nonEmpty
		}
		val maybeWinner = boards.find(_.hasWon)
		val maybeSum = maybeWinner.map(_.sumOfUnmarked)
		println(boards.mkString("\n---------------\n"))
		println(s"Winner ?\n${maybeWinner}")
		println(s"Trigger ?\n${maybeWinnerTrigger}")
		println(s"Sum of unmarked: ${maybeSum}")
		for {
			sum <- maybeSum
			winnerTrigger <- maybeWinnerTrigger
			winner <- maybeWinner
		} yield (winnerTrigger, winner, sum * winnerTrigger)
	}

	def puzzleOne = {
		getInputSourceOf(2021, 4).map(_.getLines()).map { lines =>
			val (rndSeq, boards) = getRndSeqAndBoards(lines)
			puzzleOneCore(rndSeq, boards)
		}
	}

	def puzzleTwo = {
		getInputSourceOf(2021, 4).map(_.getLines()).map { lines =>
			val (rndSeq, candidatesBoards) = getRndSeqAndBoards(lines)
			val boards: ListBuffer[Board] = ListBuffer.from(candidatesBoards)
			var maybeResults = puzzleOneCore(rndSeq, boards.toSeq)
			while(boards.length > 1) {
				maybeResults.foreach { res =>
					val winner = boards.remove(boards.indexOf(res._2))
					println(s"Removed board \n$winner\n---------------")
				}
				maybeResults = puzzleOneCore(rndSeq, boards.toSeq) // TODO optimization : remove used rnd numbers
			}
			maybeResults
		}
	}

	println(s"Day4.1: ${puzzleOne}")
	println(s"Day4.2: ${puzzleTwo}")
}
