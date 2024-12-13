package org.jro.adventofcode.y2024

import org.jro.adventofcode
import org.jro.adventofcode.Error.{IOError, WrongFixedSplit, WrongSplitError, WrongVariableSplit}

import scala.util.Using
/**
 * @author joe_mojo.
 *         2024/12/11
 */
object Day5 {

	case class Rule(before: Int, after: Int) {
		def isApplicableTo(update: Update): Boolean = {
			update.pages.contains(before) && update.pages.contains(after)
		}
		def accept(update: Update): Boolean = {
			!isApplicableTo(update) || update.pages.indexOf(before) < update.pages.indexOf(after)
		}
	}
	object Rule {
		def parse(ruleStr: String): Rule = {
			val parts = ruleStr.split('|')
			Rule(parts(0).toInt, parts(1).toInt)
		}
	}

	case class Update(pages: IndexedSeq[Int]) {
		def middle: Int = pages(pages.length / 2)
	}

	object Update {
		def parse(updateStr: String): Update = {
			Update(updateStr.split(',').map(_.toInt))
		}
	}
	
	def parseRules(rulesStr: String): IndexedSeq[Rule] = {
		rulesStr.split('\n').map(Rule.parse)
	}
	
	def parseUpdates(updatesStr: String): IndexedSeq[Update] = {
		updatesStr.split('\n').map(Update.parse)
	}


	def puzzle1(rules: IndexedSeq[Rule], updates: IndexedSeq[Update]): Int = {
		updates.filter(update => rules.forall(_.accept(update))).map(_.middle).sum
	}

	def puzzle2(rules: IndexedSeq[Rule], updates: IndexedSeq[Update]): Int = {
		val ruleSet = rules.toSet
		updates.filter(update => rules.exists(rule => !rule.accept(update))).map { update =>
			Update(update.pages.sortWith { (p1, p2) =>
				if (ruleSet.contains(Rule(p1, p2))) true
				else false // ruleSet is assumed to be complete, hence if there is no rule for (p1, p2) there always is one for (p2, p1)
			})
		}.map(_.middle).sum
	}


	def main(args: Array[String]): Unit = {
		adventofcode.getInputData(2024, 5).flatMap { inputData =>
			Using(inputData.source) { source =>
				source.mkString
			}.toEither.left.map(throwable => IOError(inputData.resource, throwable))
		} match
			case Right(content) =>
				content.split("\n\n").toList match {
					case rulesStr :: updatesStr :: Nil =>
						val rules = parseRules(rulesStr)
						val updates = parseUpdates(updatesStr)
						println(s"Puzzle 1 = ${puzzle1(rules, updates)}") // 5948 OK
						println(s"Puzzle 2 = ${puzzle2(rules, updates)}") // 3062 OK
					case other =>
						println(s"Error: ${WrongFixedSplit(content, "\n\n".r, 2, other.length)}")
				}
			case Left(error) =>
				println(s"Error: $error")
	}
}
