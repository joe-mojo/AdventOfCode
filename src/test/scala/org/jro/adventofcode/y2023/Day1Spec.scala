package org.jro.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

/**
 * @author joe_mojo.
 *         2023/12/02
 */
class Day1Spec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {
	private val replaceNumberWordsParameters = Table(
		("input line", "expected string", "expected number"),
		// puzzle 2 sample:
		("eightwothree", "83", 83),
		("two1nine", "29", 29),
		("abcone2threexyz", "13", 13),
		("xtwone3four", "24", 24),
		("4nineeightseven2", "42", 42),
		("zoneight234", "14", 14),
		("7pqrstsixteen","76", 76),
		// input:
		("nkzjrdqrmpztpqninetwofour1znnkd", "91", 91),
		("s5sevenxrdfr4mhpstgbjcfqckronesix", "56", 56),
		("three3ninefive", "35", 35),
		("crvhlfone7xsqhkshpsix2nine73oneighttq", "18", 18),
		("threethreeight", "38", 38)
	)


	"replaceNumberWords" - {
		"should replace words by numbers" in {
			forAll(replaceNumberWordsParameters) { (inputLine, expectedString, expectedNumber) =>
				Day1.replaceNumberWords(inputLine) should be(expectedString)
			}
		}
	}

	"Puzzle 1 sample should be 142" in {
		val sample: String =
			"""1abc2
			  |	pqr3stu8vwx
			  |	a1b2c3d4e5f
			  |	treb7uchet
			  |""".stripMargin
		Day1.puzzle1(sample.split("\\n").iterator) should be(142)
	}
	"Puzzle 2" - {
		"sample should be 281" in {
			val sample2: String =
				"""two1nine
				  |eightwothree
				  |abcone2threexyz
				  |xtwone3four
				  |4nineeightseven2
				  |zoneight234
				  |7pqrstsixteen
				  |""".stripMargin
			Day1.puzzle2(sample2.split("\\n").iterator) should be(281)
		}
		" should get the right number per line" in {
			forAll(replaceNumberWordsParameters) {(inputLine, expectedString, expectedNumber) =>
				Day1.puzzle2(Seq(inputLine).iterator) should be(expectedNumber)

			}
		}
	}

}
