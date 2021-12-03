package org.jro.adventofcode.y2021

import org.jro.adventofcode.{Result, findResource, getResourceFolder, writeToFile}

import java.nio.file.Path
import scala.io.Source

object Day1 extends App {
	def puzzleOne(inputResource: String): Result[Int] = {
		findResource(inputResource).map(Source.fromURL(_)).map { source =>
			source.getLines().map(_.toInt).sliding(2).foldLeft(0){ (increaseCount, depths) =>
				if(depths.head < depths.last) increaseCount + 1 else increaseCount
			}
		}
	}

	def puzzleTwo(inputResource: String): Result[Int] = {
		findResource(inputResource).map(Source.fromURL(_)).map { source =>
			source.getLines().map(_.toInt).sliding(3).map(window => window.sum).sliding(2).foldLeft(0){ (increaseCount, depths) =>
				if(depths.head < depths.last) increaseCount + 1 else increaseCount
			}
		}
	}

	def puzzleOneToGnuPlot(inputResource: String, outputResource: Path): Result[Unit] = {
		findResource(inputResource).map(Source.fromURL(_)).flatMap { source =>
			writeToFile(
				source.getLines().zipWithIndex.map(entry => s"${entry._2}\t${ - entry._1.toInt}\n"),
				outputResource
			)
		}
	}

	println(s"Day1.1: ${puzzleOne("2021/day1/input1")}")
	print("Writing input as gnuplot file: ")
	println(
		getResourceFolder(2021, 1).map(folderPath => folderPath.resolve("output1.data")).flatMap{ outputPath =>
			puzzleOneToGnuPlot("2021/day1/input1", outputPath).map(_ => outputPath)

		}
	)
	println(s"Day1.2: ${puzzleTwo("2021/day1/input1")}")
}
