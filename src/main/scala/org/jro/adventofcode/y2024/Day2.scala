package org.jro.adventofcode.y2024

import org.jro.adventofcode

object Day2 {
	private val MaxSAfeDiff = 3

	enum Diff:
		case Ascending, Descending, Unsafe

	private def parseLine(line: String): Seq[Int] = {
		line.split(" ").map(_.toInt)
	}

	private def isSafeDiff(val1: Int, val2: Int): Diff = {
		val diff = Math.abs(val1 - val2)
		if (diff > MaxSAfeDiff) Diff.Unsafe
		else if (val1 < val2) Diff.Ascending
		else if (val1 > val2) Diff.Descending
		else Diff.Unsafe
	}

	private def isSafeDiff(val1: Int, val2:Int, lastDiff: Diff): Diff = {
		isSafeDiff(val1, val2) match
			case Diff.Unsafe => Diff.Unsafe
			case diff if diff == lastDiff => diff
			case _ => Diff.Unsafe
	}

	private def isSafe(levels: Seq[Int]): Boolean = {
		val lvlPairs = levels.sliding(2).toIndexedSeq
		lvlPairs.drop(1).foldLeft(isSafeDiff(lvlPairs.head.head, lvlPairs.head(1))) { (lastDiff, pair) =>
			isSafeDiff(pair(0), pair(1), lastDiff)
		} != Diff.Unsafe
	}

	private def isReallySafe(levels: Seq[Int]): Boolean = {
		(for(i <- levels.indices) yield levels.slice(0, i) ++ levels.slice(i+1, levels.length)).exists(isSafe)
	}

	def puzzle1(lines: Iterator[String]): Long = {
		lines.map(parseLine).count(isSafe)
	}

	def puzzle2(lines: Iterator[String]): Long = {
		lines.map(parseLine).count(isReallySafe)
	}

	def main(args: Array[String]): Unit = adventofcode.y2024.classicMain[Long](2, puzzle1, puzzle2)
}
