package org.jro.adventofcode.y2020

import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day10Spec extends AnyFreeSpec with Matchers with Inside{
	private val smallSample: Seq[Int] = """16
								|10
								|15
								|5
								|1
								|11
								|7
								|19
								|6
								|12
								|4""".stripMargin.split("""\n""").map(_.toInt).toSeq.sorted

	private val sample: Seq[Int] = """28
						   |33
						   |18
						   |42
						   |31
						   |14
						   |46
						   |20
						   |48
						   |47
						   |24
						   |23
						   |49
						   |45
						   |19
						   |38
						   |39
						   |11
						   |1
						   |32
						   |25
						   |35
						   |8
						   |17
						   |7
						   |9
						   |4
						   |2
						   |34
						   |10
						   |3""".stripMargin.split("""\n""").map(_.toInt).toSeq.sorted

	"countHop when" - {
		s"input data is the small sample $smallSample should" - {
			"find 7 diff of 1 and 5 diff of 3" in {
				val actualStats = Day10.countHop(smallSample)
				actualStats.get(1) should contain(7)
				actualStats.get(3) should contain(5)
			}
		}
		s"input data is the second sample ${sample} should" - {
			"find 22 diff of 1 and 10 diff of 3" in {
				val actualStats = Day10.countHop(sample)
				actualStats.get(1) should contain(22)
				actualStats.get(3) should contain(10)
			}
		}
	}



}
