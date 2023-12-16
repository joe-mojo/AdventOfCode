package org.jro.adventofcode.y2023

import jdk.internal.org.jline.utils.Display
import org.jro.adventofcode
import org.jro.adventofcode.getInputLinesOrThrow
import org.jro.adventofcode.y2023.Day5.*

import java.util.concurrent.Executors
import scala.collection.immutable.NumericRange
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

case class Day5(seeds: Set[Int], steps: Seq[Step]) {

}

object Day5 {

	case class Step(mappings: Set[Mapping]) {
		def map(srcValue: Long): Long = {
			mappings.find(_.isDefinedAt(srcValue)).fold(srcValue)(_(srcValue))
		}
	}
	case class Mapping(dstStart: Long, srcStart: Long, range: Long) extends PartialFunction[Long, Long] {
		val dstEnd: Long = dstStart + range - 1
		val srcEnd: Long = srcStart + range - 1
		val offset: Long = dstStart - srcStart

		override def apply(srcValue: Long): Long = srcValue + offset

		override def isDefinedAt(value: Long): Boolean = {
			value >= srcStart && value <= srcEnd
		}
	}

	case class Input1(seeds: Seq[Long], steps: Seq[Step]) {
		def toInput2: Input2 = Input2(seeds.sliding(2, 2).map(seedDef => seedDef(0) until (seedDef(0) + seedDef(1))).toSeq, steps)

	}
	case class Input2(seedRanges: Seq[NumericRange.Exclusive[Long]], steps: Seq[Step])

	def parseLines(lines: Iterable[String]): Input1 = {
		val seeds = lines.head.split("""\s""").tail.map(_.toLong).toSeq
		println(s"Parsed seeds: $seeds")

		Input1(seeds= seeds, steps = StepsParser.parseSteps(lines.tail))
	}

	object StepsParser {
		val NewStep: Regex = """^[a-z- ]+map:$""".r
		val stepsAccumulator: ListBuffer[Step] = ListBuffer.empty
		val mappingAccumulator: ListBuffer[Mapping] = ListBuffer.empty

		private def createStep(): Unit = {
			if (mappingAccumulator.nonEmpty) {
				stepsAccumulator += Step(mappingAccumulator.toSet)
				mappingAccumulator.clear()
			}
		}

		private def parseLine(line: String): Unit = {
			line match
				case "" => ()
				case NewStep() =>
					createStep()
				case mappingString =>
					val values = mappingString.split(" ").map(_.toLong)
					mappingAccumulator += Mapping(values.head, values(1), values(2))
		}

		def parseSteps(lines: Iterable[String]): Seq[Step] = {
			lines.foreach(parseLine)
			createStep()
			val res = stepsAccumulator.toIndexedSeq
			stepsAccumulator.clear()
			res
		}

	}

	def findLocations(seeds: Iterator[Long], steps:Seq[Step], display: Boolean): Iterator[Long] = {
		seeds.map { seed =>
			val location = steps.foldLeft(seed) { (value, step) =>
				step.map(value)
			}
			if (display) println(s"seed $seed --> location $location")
			location
		}
	}

	def puzzle1(input: Input1): Long = {
		val locations: Seq[Long] = findLocations(input.seeds.iterator, input.steps, true).toSeq
		locations.min
	}

	def splitRange(range: NumericRange.Exclusive[Long]): Seq[NumericRange.Exclusive[Long]] = {
		val middle = range.start + ((range.end - range.start) / 2L)
		Seq(
			range.start until middle,
			middle until range.end
		)
	}

	def puzzle2(input: Input2): Long = {
		implicit val ec: scala.concurrent.ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(12))
		val splitRanges: Seq[NumericRange.Exclusive[Long]] = input.seedRanges.flatMap(splitRange)
		println(s"Input2 ranges to run: $splitRanges")
		val minimums: Seq[Future[Long]] = splitRanges.map { range =>
			Future {
				findLocations(range.iterator, input.steps, false).min
			}.map { min =>
				println(s"Found min for range $range = $min")
				min
			}
		}
		Await.result(Future.sequence(minimums).map(_.min), Duration.Inf)
	}



	def main(args: Array[String]): Unit = {
		val sampleLines = """seeds: 79 14 55 13
					   |
					   |seed-to-soil map:
					   |50 98 2
					   |52 50 48
					   |
					   |soil-to-fertilizer map:
					   |0 15 37
					   |37 52 2
					   |39 0 15
					   |
					   |fertilizer-to-water map:
					   |49 53 8
					   |0 11 42
					   |42 0 7
					   |57 7 4
					   |
					   |water-to-light map:
					   |88 18 7
					   |18 25 70
					   |
					   |light-to-temperature map:
					   |45 77 23
					   |81 45 19
					   |68 64 13
					   |
					   |temperature-to-humidity map:
					   |0 69 1
					   |1 0 69
					   |
					   |humidity-to-location map:
					   |60 56 37
					   |56 93 4""".stripMargin.split("""\n""").toIndexedSeq
		val sample = parseLines(sampleLines)
		println(s"Puzzle1 with sample = ${puzzle1(sample)}")
		val input: Input1 = parseLines(getInputLinesOrThrow(2023, 5))
		println(s"Puzzle1 with input = ${puzzle1(input)}") // 88151870
		println()

		println(s"Puzzle2 with sample = ${puzzle2(sample.toInput2)}")
		println(s"Puzzle2 with input = ${puzzle2(input.toInput2)}")
	}
}
