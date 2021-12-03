package org.jro.adventofcode.y2020

import org.jro.adventofcode
import org.jro.adventofcode.y2020.Day7.{BagColor, Content, Rule}
import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day7Spec extends AnyFreeSpec with Matchers with Inside {
	val sampleInput = """light red bags contain 1 bright white bag, 2 muted yellow bags.
						|dark orange bags contain 3 bright white bags, 4 muted yellow bags.
						|bright white bags contain 1 shiny gold bag.
						|muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
						|shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
						|dark olive bags contain 3 faded blue bags, 4 dotted black bags.
						|vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
						|faded blue bags contain no other bags.
						|dotted black bags contain no other bags.""".stripMargin

	val sampleInput2 = """shiny gold bags contain 2 dark red bags.
						 |dark red bags contain 2 dark orange bags.
						 |dark orange bags contain 2 dark yellow bags.
						 |dark yellow bags contain 2 dark green bags.
						 |dark green bags contain 2 dark blue bags.
						 |dark blue bags contain 2 dark violet bags.
						 |dark violet bags contain no other bags.""".stripMargin

	/*"Rule regex" - {
		"should match rule string and extract data" - {
			"when there is only 2 contents" in {
				val firstLine = sampleInput.split("\n").head
				inside(firstLine) {
					case Day7.Rule.Container(containerColor, content@_*) =>
						containerColor shouldBe "light red"
						content should contain theSameElementsInOrderAs Seq("1", "bright white", "2", "muted yellow")
				}
			}
			"when there is more than 2 contents" in {
				val firstLine = "light violet bags contain 2 striped magenta bags, 5 light lime bags, 5 posh cyan bags."
				Day7.Rule.Container.findAllMatchIn(firstLine).foreach { regexMatch =>
					println(s"""Found: "$regexMatch"""")
					(0 to regexMatch.groupCount).map(regexMatch.group).foreach { matchGroup =>
						println(s"""\tfound group: "$matchGroup"""")

					}
				}
				inside(firstLine) {
					case Day7.Rule.Container(containerColor, content@_*) =>
						containerColor shouldBe "light violet"
						content should contain theSameElementsInOrderAs Seq("2", "striped magenta", "5", "light lime", "5", "posh cyan")
				}
			}
		}
	}*/
	"Rule parsing" - {
		"should parse sample lines into rules" in {
			val actualMaybeRules = Day7.parseRules(sampleInput.split("\n").iterator)

			inside(actualMaybeRules) {
				case Right(rules) =>
					println(rules.mkString("\n"))
					rules.head shouldBe Rule(BagColor("light red"), Seq(Content(1,BagColor("bright white")), Content(2,BagColor("muted yellow"))))
					rules.last shouldBe Rule(BagColor("dotted black"), Seq())
			}
		}
		"should parse input lines into rules" in {
			val actualMaybeRules = adventofcode.getInputSourceOf(2020, 7).map(_.getLines()).flatMap(Day7.parseRules)

			inside(actualMaybeRules) {
				case Right(rules) =>
					rules should have length(594)
					rules.last shouldBe Rule(BagColor("shiny maroon"), Seq(Content(2, BagColor("dim crimson"))))
			}
		}
	}
	"Rules" - {
		"should find the direct containers of a bag" in {
			val actualMaybeRules = Day7.parseRules(sampleInput.split("\n").iterator)
			inside(actualMaybeRules) {
				case Right(rules) =>
					Day7.Rules.findDirectContainers(BagColor("shiny gold"), rules) should contain theSameElementsAs Seq(
						BagColor("bright white"),
						BagColor("muted yellow"),
					)
			}
		}
		"should find all containers of a bag" in {
			val actualMaybeRules = Day7.parseRules(sampleInput.split("\n").iterator)
			inside(actualMaybeRules) {
				case Right(rules) =>
					Day7.Rules.findAllContainers(BagColor("shiny gold"), rules) should contain theSameElementsAs Set(
						BagColor("bright white"),
						BagColor("muted yellow"),
						BagColor("dark orange"),
						BagColor("light red")
					)
			}
		}
	}
}
