package org.jro.adventofcode.y2020

import org.jro.adventofcode.y2020.Day2.PasswdEntry
import org.jro.adventofcode.y2020.Day2.PasswdEntry.{isValidForPuzzle1, parseIntoPasswdEntry}
import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day2Spec extends AnyFreeSpec with Matchers with Inside {

	private val inputLineWithInvalidENtry = "13-15 h: hhhrhhlhhpjhhhr";
	private val inputLineWithValidEntry = "1-9 k: dmwkkkkzk";

	"PasswordEntry" - {
		s"""for invalid puzzle 1 entry "$inputLineWithInvalidENtry"""" - {
			"""should be parsed with right values""" in {
				val actual = parseIntoPasswdEntry(inputLineWithInvalidENtry)
				inside(actual) {
					case Right(pe@PasswdEntry('h', 13, 15, "hhhrhhlhhpjhhhr")) =>
						isValidForPuzzle1(pe) shouldBe false
				}
			}
		}
		s"""for valid puzzle 1 entry "$inputLineWithValidEntry"""" - {
			"""should be parsed with right values""" in {
				val actual = parseIntoPasswdEntry(inputLineWithValidEntry)
				inside(actual) {
					case Right(pe@PasswdEntry('k', 1, 9, "dmwkkkkzk")) =>
						isValidForPuzzle1(pe) shouldBe true
				}
			}
		}

	}



}
