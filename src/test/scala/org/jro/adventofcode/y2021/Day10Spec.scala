package org.jro.adventofcode.y2021

import org.jro.adventofcode.y2021.Day10.{IncompleteInput, ParseError}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day10Spec extends AnyFreeSpec with Matchers {
	"checkLine" - {
		"when called with a valid line" - {
			"should return None" in {
				Day10.checkLine("[]") shouldBe empty
				Day10.checkLine("<>") shouldBe empty
				Day10.checkLine("{}") shouldBe empty
				Day10.checkLine("()") shouldBe empty
				Day10.checkLine("[<>({}){}[([])<>]]") shouldBe empty
				Day10.checkLine("(((((((((())))))))))") shouldBe empty
				Day10.checkLine("<([{}])>") shouldBe empty
				Day10.checkLine("([])") shouldBe empty
				Day10.checkLine("{()()()}") shouldBe empty
			}
		}
		"should return a ParseError" - {
			"when called with a corrupted line" in {
				Day10.checkLine("(]") should contain(ParseError("(]", 1, "]", ")"))
				Day10.checkLine("{()()()>") should contain(ParseError("{()()()>", 7, ">", "}"))
				Day10.checkLine("(((()))}") should contain(ParseError("(((()))}", 7, "}", ")"))
				Day10.checkLine("<([]){()}[{}])") should contain(ParseError("<([]){()}[{}])", 13, ")", ">"))
				Day10.checkLine("{([(<{}[<>[]}>{[]{[(<()>") should contain(ParseError("{([(<{}[<>[]}>{[]{[(<()>", 12, "}", "]"))
				Day10.checkLine("[[<[([]))<([[{}[[()]]]") should contain(ParseError("[[<[([]))<([[{}[[()]]]", 8, ")", "]"))
			}
			"when called with an incomplete line" in {
				Day10.checkLine("((([]))){") should contain(IncompleteInput("((([]))){", "{"))
			}
		}
	}
}
