package org.jro.adventofcode.y2019

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class Day1Spec extends AnyFreeSpec with Matchers {
	"requiredFuelForModuleOf" - {
		"should compute fuel from mass  as m / 3 -2 " in {
			Day1.requiredFuelForModuleOf(12) shouldBe 2
			Day1.requiredFuelForModuleOf(14) shouldBe 2
			Day1.requiredFuelForModuleOf(1969) shouldBe 654
			Day1.requiredFuelForModuleOf(100756) shouldBe 33583
		}
	}

	"requiredFuelForModuleAndFuel" - {
		"should compute required fuel for module and fuel" in {
			Day1.requiredFuelForModuleAndFuel(14) shouldBe 2
			Day1.requiredFuelForModuleAndFuel(1969) shouldBe 966
			Day1.requiredFuelForModuleAndFuel(100756) shouldBe 50346

		}
	}

	"requiredFuelForModules" - {
		"when not accounting for fuel" - {
			"should compute unit requirements and sum them" in {
				Day1.requiredFuelForModules(Day1.requiredFuelForModuleOf)(Seq(12, 14, 1969, 100756).iterator) shouldBe (2 + 2 + 654 + 33583)
			}
		}
		"when accounting for fuel mass" - {
			Day1.requiredFuelForModules(Day1.requiredFuelForModuleAndFuel)(Seq(14, 1969, 100756).iterator) shouldBe (2 + 966 + 50346)
		}
	}


}
