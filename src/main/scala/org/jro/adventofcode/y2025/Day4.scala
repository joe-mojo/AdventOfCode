package org.jro.adventofcode.y2025

import org.jro.adventofcode
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Seq => MSeq}

/**
 * @author joe_mojo.
 *         2025/12/04
 */
object Day4 {

	class Grid(val cells: IndexedSeq[IndexedSeq[Boolean]]) {
		def cellAt(row: Int, col: Int): Boolean = cells(row)(col)
		def countNeighbors(row: Int, col: Int): Int = {
			val neighborOffsets = Seq(
				(-1, -1), (-1, 0), (-1, 1),
				(0, -1),          (0, 1),
				(1, -1),  (1, 0), (1, 1)
			)
			neighborOffsets.count {
				case (rowOffset, colOffset) =>
					val neighborRow = row + rowOffset
					val neighborCol = col + colOffset
					neighborRow >= 0 && neighborRow < cells.length &&
					neighborCol >= 0 && neighborCol < cells.head.length &&
					cells(neighborRow)(neighborCol)
			}
		}
		def countAccessibleRolls: Int = {
			cells.indices.flatMap { row =>
				cells.head.indices.map { col =>
					if (cellAt(row, col) && countNeighbors(row, col) < 4) 1 else 0
				}
			}.sum
		}
	}
	object Grid {
		def parseLine(line: String): IndexedSeq[Boolean] = line.map {
			case '@' => true
			case _ => false
		}
		def parseLines(lines: Iterator[String]): Grid = new Grid(lines.map(parseLine).toIndexedSeq)
	}

	class WorkingGrid(private val cells: Array[Array[Boolean]]) {
		def cellAt(row: Int, col: Int): Boolean = cells(row)(col)
		def cellAt(row: Int, col: Int, cell: Boolean): WorkingGrid = {
			cells(row)(col) = cell
			this
		}

		def countNeighbors(row: Int, col: Int): Int = {
			val neighborOffsets = Seq(
				(-1, -1), (-1, 0), (-1, 1),
				(0, -1), (0, 1),
				(1, -1), (1, 0), (1, 1)
			)
			neighborOffsets.count {
				case (rowOffset, colOffset) =>
					val neighborRow = row + rowOffset
					val neighborCol = col + colOffset
					neighborRow >= 0 && neighborRow < cells.length
							&&
							neighborCol >= 0 && neighborCol < cells.head.length &&
							cells(neighborRow)(neighborCol)
			}
		}

		private def findRemovableCells: ListBuffer[(Int, Int)] = {
			val removableCells = ListBuffer[(Int, Int)]()
			for (row <- cells.indices; col <- cells.head.indices) {
				if (cellAt(row, col) && countNeighbors(row, col) < 4) {
					removableCells += (row -> col)
				}
			}
			removableCells
		}
		private def removeMarkedCells(cellsToRemove: scala.collection.Seq[(Int, Int)]): WorkingGrid = {
			cellsToRemove.foreach {
				case (row, col) => cellAt(row, col, false)
			}
			this
		}

		def unloadRolls(): Int = {
			var totalRemoved = 0
			var removableCells = findRemovableCells
			while (removableCells.nonEmpty) {
				totalRemoved += removableCells.length
				removeMarkedCells(removableCells)
				removableCells = findRemovableCells
			}
			totalRemoved
		}
	}
	object WorkingGrid {
		def fromGrid(grid: Grid): WorkingGrid = {
			val cellsArray = Array.ofDim[Boolean](grid.cells.length, grid.cells.head.length)
			for (row <- grid.cells.indices; col <- grid.cells.head.indices) {
				cellsArray(row)(col) = grid.cellAt(row, col)
			}
			new WorkingGrid(cellsArray)
		}
	}


	def parseInput(input: adventofcode.InputData): Either[adventofcode.Error, Grid] = Right(Grid.parseLines(input.source.getLines()))
	def puzzle1(input: Grid): Int = input.countAccessibleRolls
	def puzzle2(input: Grid): Int = WorkingGrid.fromGrid(input).unloadRolls()


	def main(args: Array[String]): Unit = adventofcode.y2025.mainWithTransformer(4, puzzle1 /* OK: 1493 */, puzzle2 /* OK: 9194 */, parseInput)
}
