package org.jro.adventofcode.y2020

import org.jro.adventofcode.{Error, getInputSourceOf}
import org.jro.adventofcode.Error.{NaN, NotMatching}

import scala.util.Try
import scala.util.matching.Regex

object Day7 {
	case class BagColor(color: String)

	case class Content(count: Int, bag: BagColor)
	object Content {
		def fromTokens(countToken: String, colorToken: String): Either[Error, Content] = {
			Try(countToken.toInt).fold(
				err => Left.apply(NaN(countToken)),
				count => Right(Content(count, BagColor(colorToken.trim.toLowerCase)))
			)
		}
	}

	case class Rule(container: BagColor, content: Seq[Content])
	object Rule {
		val Pattern: Regex = """([a-z]+ [a-z]+) bags contain ((?:\d+ [a-z]+ [a-z]+ bags?(?:, |.))+)""".r
		val ContentElement: Regex = """(\d+) ([a-z]+ [a-z]+) bags?(?:, |.)""".r
		val Leaf: Regex = """([a-z]+ [a-z]+) bags contain no other bags\.""".r

		def fromLine(value: String): Either[Error, Rule] = {
			value match {
				case Pattern(bagColor, contentElts) =>
					Error.sequence(ContentElement.findAllMatchIn(contentElts).map { regexMatch =>
						if(regexMatch.groupCount == 2) Content.fromTokens(regexMatch.group(1), regexMatch.group(2))
						else Left(NotMatching(contentElts, ContentElement))
					}).map(Rule(BagColor(bagColor), _))
				case Leaf(bagColor) =>
					Right(Rule(BagColor(bagColor), Seq.empty[Content]))
				case other =>
					Left(NotMatching(value, Pattern))
			}
		}
	}

	object Rules {
		def findDirectContainers(bag: BagColor, inRules: Seq[Rule]): Set[BagColor] = {
			inRules.filter(rule => rule.content.exists(_.bag == bag)).map(_.container).toSet
		}

		def findAllContainers(bagColor: BagColor, inRules: Seq[Rule]): Set[BagColor] = {
			def doFind(bagColors: Set[BagColor], inRecRules: Seq[Rule]): Set[BagColor] = {
				if(bagColors.isEmpty) Set.empty
				else {
					val directContainers = bagColors.flatMap(findDirectContainers(_, inRecRules))
					directContainers ++ doFind(directContainers, inRecRules)
				}
			}
			doFind(Set(bagColor), inRules)
		}

		def createRuleMap(inRules: Seq[Rule]): Map[BagColor, Seq[Content]] = {
			inRules.map(rule => rule.container -> rule.content).toMap
		}

		def countAllBagsIn(bagColor: BagColor, inRuleMap: Map[BagColor, Seq[Content]]): Int = {
			inRuleMap.get(bagColor).map { contents =>
				if(contents.isEmpty) 0
				else {
					contents.map(content => content.count * (1 + countAllBagsIn(content.bag, inRuleMap))).sum
				}
			}.getOrElse(0)
		}
	}

	def parseRules(lines: Iterator[String]): Either[Error, Seq[Rule]] = Error.sequence(lines.map(Rule.fromLine))

	def main(args: Array[String]): Unit = {
		val maybeRules: Either[Error, Seq[Rule]] = getInputSourceOf(2020, 7).map(_.getLines())
			.flatMap(parseRules)

		val res1: Either[Error, Set[BagColor]] = maybeRules.map(Rules.findAllContainers(BagColor("shiny gold"), _))
		res1 match {
			case Left(err) => println(s"Puzzle1 err: $err")
			case Right(containers) => println(s"container count: ${containers.size}")
		}

		val res2: Either[Error, Int] = maybeRules.map { ruleSeq =>
			Rules.countAllBagsIn(BagColor("shiny gold"), Rules.createRuleMap(ruleSeq))
		}
		println(s"""Bag count in "shiny gold" bag = $res2 """)
	}
}
