package org.jro.adventofcode.y2023

import org.jro.adventofcode.y2023.Day5.{Mapping, Step}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day5Spec extends AnyFreeSpec with Matchers {

	"Day5" - {
		"Mapping should map according to range" in {
			val mapping = Mapping(50, 98, 2)

			mapping.apply(98) should be(50)
			mapping.apply(99) should be(51)
			for {i <- 0 to 97} {
				mapping.isDefinedAt(i) should be(false)
			}
			for {i <- 100 to 200} {
				mapping.isDefinedAt(i) should be(false)
			}

		}

		"Step should map according to ranges" in {
			val step = Step(Set(Mapping(50, 98, 2), Mapping(52, 50, 48)))

			// no mapping from 0 to 49
			for {i <- 0 until 50} {
				step.map(i) should be(i)
			}

			// Mapping(52, 50, 48)
			for {i <- 50 until (50 + 48)} {
				step.map(i) should be(i+2)
			}

			// Mapping(50, 98, 2)
			step.map(98) should be(50)
			step.map(99) should be(51)

			// no mapping from 100 to ...
			for {i <- 100 until 200 } {
				step.map(i) should be(i)
			}

		}

	}



}
