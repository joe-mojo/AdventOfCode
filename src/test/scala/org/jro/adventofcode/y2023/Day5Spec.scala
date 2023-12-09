package org.jro.adventofcode.y2023

import org.jro.adventofcode.y2023.Day5.Mapping
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day5Spec extends AnyFreeSpec with Matchers {

	"Day5 Mapping should map according to range" in {
		val mapping = Mapping(50, 98, 2)

		mapping.apply(98) should be(50)
		mapping.apply(99) should be(51)
		for{i <- 0 to 97} {
			mapping.apply(i) should be(i)
		}
		for {i <- 100 to 200} {
			mapping.apply(i) should be(i)
		}

	}

}
