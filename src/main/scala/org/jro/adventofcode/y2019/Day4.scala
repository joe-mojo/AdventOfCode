package org.jro.adventofcode.y2019

import scala.annotation.tailrec

object Day4 {
	val Min = 152085
	val Max = 670283

	def containsPair(i: Int): Boolean = {
		//Day4_1 i.toString.sliding(2, 1).exists(str => str(0) == str(1))
		countDups(i).exists(_.count == 2)
	}

	case class DupCount(digit: Char, count: Int)

	def countDups(number: Int): Seq[DupCount] = {
		@tailrec
		def countDigitDups(digits: Seq[Char], dups: Seq[DupCount], previous: Char): Seq[DupCount] = {
			digits.toList match {
				case Nil =>
					dups
				case head :: tail if head != previous =>
					countDigitDups(tail, DupCount(head, 1) +: dups, head)
				case head :: tail =>
					countDigitDups(tail, dups.head.copy(count = dups.head.count + 1) +: dups.tail, head)
			}
		}
		countDigitDups(number.toString, Seq.empty[DupCount], '\u0000').reverse
	}

	def hasIncreasingDigits(i: Int): Boolean = {
		i.toString.foldLeft((0, true)) { (status, n) =>
			if(status._2 &&  n>= status._1) (n, true)
			else (status._1, false)
		}._2
	}

	def main(args: Array[String]): Unit = {
		val nbPasswds = Min to Max count { n: Int =>
			containsPair(n) && hasIncreasingDigits(n)
		}
		println(s"Day4-1 answer: $nbPasswds passwords between $Min and $Max") //Day4-1: 1764
	}
}
