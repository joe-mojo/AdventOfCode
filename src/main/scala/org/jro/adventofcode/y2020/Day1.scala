package org.jro.adventofcode.y2020

import org.jro.adventofcode._

import scala.io.Source
import scala.collection.{mutable => m}

object Day1 extends App {

	case class PuzzleOneEntry(amount1: Int, amount2: Int) {
		lazy val sum: Int = amount1 + amount2
		def result = amount1 * amount2
	}

	case class Puzzle2Entry(amounts: Seq[Int]) {
		val sum: Int = amounts.sum
		def result: Long = amounts.product
	}


	def puzzleOne(inputResource: String): Result[Option[PuzzleOneEntry]] = {
		findResource(inputResource).map(Source.fromURL(_)).map { source =>
			source.getLines().map(_.toInt).toList.combinations(2).map { amounts =>
				PuzzleOneEntry(amounts.head, amounts.tail.head)
			}.find(_.sum == 2020)
		}
	}

	def combine(list: Seq[Int], n: Int = 3): Seq[Seq[Int]] = {
		if(n == 0 ) Seq(Seq.empty[Int])
		else {
			list.zipWithIndex.flatMap { entry =>
				val (e, i) = entry
				combine(list.slice(i + 1, list.length), n - 1).map(c => Seq(e) ++ c)
			}
		}
	}

	def puzzleTwo(inputResource: String): Result[Option[Seq[Int]]] = {
		findResource(inputResource).map(Source.fromURL(_)).map { source =>
			combine(source.getLines().map(_.toInt).toSeq).find(_.sum == 2020)
		}
	}


	/*println(puzzleOne("2020/day1/input1") match {
		case Right(Some(res)) => s"${res}.result = ${res.result}"
		case other => other.toString
	})*/

	println((puzzleTwo("2020/day1/input1"): @unchecked) match {
		case Right(Some(res2)) => println(s"$res2 : sum = ${res2.sum} ; product = ${res2.product}")
	}
	)

}
