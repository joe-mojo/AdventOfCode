package org.jro.adventofcode.y2025

import org.jro.adventofcode.y2025.classicMain

object Day7 {

	private[y2025] class BeamExtenderInfo()

	private[y2025] def extendBean(linePair: Seq[String]): (String, Int) = {
		linePair.head.zip(linePair.last).zipWithIndex.foldLeft(linePair.last -> 0){ (res, charPaiWithIdx) =>
			charPaiWithIdx match {
				case (('S', '.'), idx) => res._1.updated(idx, '|') -> res._2
				case (('|', '.'), idx) => res._1.updated(idx, '|') -> res._2
				case (('|', '^'), idx) if idx > 0 && idx < (linePair.head.length - 1) => res._1.updated(idx - 1, '|').updated(idx + 1, '|') -> (res._2 + 1)
				case _ => res
			}
		}
	}

	private[y2025] def scanBeam(input: Iterator[String]): Int = {
		input.sliding(2).map(linePair => {
			val (newSecondLine, splitCount) = extendBean(linePair)
			splitCount
		}).sum
	}

	def puzzle1(input: Iterator[String]): Int = {
		scanBeam(input)
	}

	def main(args: Array[String]): Unit = classicMain(7, puzzle1, (iter) => ???)
}
