package org.jro.adventofcode.y2021

import org.jro.adventofcode

import scala.collection.immutable.ListMap

object Day14 {
	val LineRegex = """([A-Z][A-Z]) -> ([A-Z])""".r
	def parseLine(line: String): (String, Char) = {
		line match {
			case LineRegex(pair, insert) => (pair, insert.head)
		}
	}

	def step(currentPolymer: ListMap[String, Long], pairMap: Map[String, Char]): ListMap[String, Long] = {
		currentPolymer.toSeq.flatMap {
			case (pair, count) => pairMap.get(pair).fold(Seq(pair -> count)) { c =>
				Seq(s"${pair.head}$c" -> count, s"$c${pair.last}" -> count)
			}
		}.foldLeft(ListMap.empty[String, Long]) {(newPolymer, entry) =>
			newPolymer.updatedWith(entry._1) {
				case Some(count) => Some(count + entry._2)
				case None => Some(entry._2)
			}
		}
	}

	def createTemplateLM(line: String): ListMap[String, Long] = {
		line.sliding(2, 1).foldLeft(ListMap.empty[String, Long]) { (templateMap, pair) =>
			templateMap.updatedWith(pair){
				case Some(count) => Some(count + 1)
				case None => Some(1)
			}
		}
	}

	def polymerMapToCharFreq(currentPolymer: ListMap[String, Long]): Map[Char, Long] = {
		currentPolymer.toSeq.foldLeft(Map.empty[Char, Long]){ (charFreqs, pairFreq) =>
			charFreqs.updatedWith(pairFreq._1.head) {
				case Some(count) => Some(count + pairFreq._2)
				case None => Some(pairFreq._2)
			}
		}
	}

	def puzzle1(template: String, pairMap: Map[String, Char], steps: Int): Long = {
		val templateLM: ListMap[String, Long] = createTemplateLM(template)
		println(s"Template=$templateLM")
		println(s"pairMap=${pairMap}")
		val res = (1 to steps).foldLeft(templateLM) { (currentPolyLM, stepNum) =>
			val res = step(currentPolyLM, pairMap)
			println(s"Step $stepNum: \n\t$res")
			res
		}
		val charFreqs = polymerMapToCharFreq(res).updatedWith(template.last) {
			case Some(count) => Some(count + 1)
			case None => Some(1)
		}
		println(s"Char frequencies: ${charFreqs}")

		charFreqs.values.max - charFreqs.values.min
	}

	def main(args: Array[String]): Unit = {
		val sampleTemplateLM: ListMap[String, Long] = createTemplateLM("NNCB")
		val samplePairMap: Map[String, Char] = """CH -> B
							 |HH -> N
							 |CB -> H
							 |NH -> C
							 |HB -> C
							 |HC -> B
							 |HN -> C
							 |NN -> C
							 |BH -> H
							 |NC -> B
							 |NB -> B
							 |BN -> B
							 |BB -> N
							 |BC -> B
							 |CC -> N
							 |CN -> C""".stripMargin.split("\\n").map(parseLine).toMap

		val sampleRes = (1 to 10).foldLeft(sampleTemplateLM) { (currentPolyLM, stepNum) =>
			val res = step(currentPolyLM, samplePairMap)
			println(s"sample step $stepNum: \n\t$res")
			res
		}
		println(s"Char frequencies: ${polymerMapToCharFreq(sampleRes)}")

		println()

		adventofcode.getInputLines(2021, 14).map { lines =>
			val template = lines.next()
			val pairMap: Map[String, Char] = lines.drop(1).map(parseLine).toMap
			println(s"Day14.1: ${puzzle1(template, pairMap, 10)}")
			println(s"Day14.2: ${puzzle1(template, pairMap, 40)}")
		}

	}
}
