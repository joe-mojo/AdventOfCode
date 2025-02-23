package org.jro.adventofcode.y2024

import org.jro.adventofcode.y2024.Day12.{Coords, Field, GardenPlot, LocatedPlow, createPlowsMap}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/**
 * @author joe_mojo.
 *         2025/02/23
 */
class Day12Spec extends AnyFreeSpec with Matchers {
	private val sampleInput = """AAAA
						|BBCD
						|BBCC
						|EEEC""".stripMargin.split("\n").toIndexedSeq

	private val largeSampleInput = """RRRRIICCFF
							 |RRRRIICCCF
							 |VVRRRCCFFF
							 |VVRCCCJFFF
							 |VVVVCJJCFE
							 |VVIVCCJJEE
							 |VVIIICJJEE
							 |MIIIIIJJEE
							 |MIIISIJEEE
							 |MMMISSJEEE""".stripMargin.split("\n").toIndexedSeq

	private val multipleOneSizedPlotsSampleInput = """OOOOO
											 |OXOXO
											 |OOOOO
											 |OXOXO
											 |OOOOO""".stripMargin.split("\n").toIndexedSeq

	private val subreditXloopSampleInput = """AAXXXXXXBB
									 |AXXDDDEEXX
									 |XXDDCCCEXX
									 |XXDDCCCEEE
									 |XXCCXXXXEE
									 |XXCCFFGXXE
									 |HXXFFGGXXH
									 |HHXXXXXXHH""".stripMargin.split("\n").toIndexedSeq

	"Day12" - {
		"createPlows" - {
			"should create plows from sample input" in {
				val plows = createPlowsMap(sampleInput)
				plows should have size 16
				plows should contain key Coords(0, 0)
				plows should contain key Coords(0, 1)
				plows should contain key Coords(0, 2)
				plows should contain key Coords(0, 3)
				plows.get(Coords(1, 3)) should contain(LocatedPlow(Coords(1, 3), 'E', 2, Set(Coords(0, 3), Coords(2, 3))))
			}
		}

		"createField" - {
			"should create field from sample input" in {
				val locatedPlows = createPlowsMap(sampleInput)
				val field = Field(locatedPlows)
				field.plows should have size 16
				field.gardenPlots should have size 5
				field.gardenPlots.foreach{ gardenPlot =>
					gardenPlot.id match
						case "A0000000000" =>
							gardenPlot.area shouldBe 4
							gardenPlot.fences shouldBe 10
							gardenPlot.fenceCost shouldBe 40
						case "B0000000000" =>
							gardenPlot.area shouldBe 4
							gardenPlot.fences shouldBe 8
							gardenPlot.fenceCost shouldBe 32
						case "C0000000000" =>
							gardenPlot.area shouldBe 4
							gardenPlot.fences shouldBe 10
							gardenPlot.fenceCost shouldBe 40
						case "D0000000000" =>
							gardenPlot.area shouldBe 1
							gardenPlot.fences shouldBe 4
							gardenPlot.fenceCost shouldBe 4
						case "E0000000000" =>
							gardenPlot.area shouldBe 3
							gardenPlot.fences shouldBe 8
							gardenPlot.fenceCost shouldBe 24
				}
				field.fenceCost shouldBe 140
			}
			"should create field from large sample input" in {
				val locatedPlows = createPlowsMap(largeSampleInput)
				val field = Field(locatedPlows)
				field.plows should have size 100
				field.gardenPlots should have size 11
				field.gardenPlots.foreach{ gardenPlot =>
					gardenPlot.id match
						case "R0000000000" =>
							gardenPlot.area shouldBe 12
							gardenPlot.fences shouldBe 18
							gardenPlot.fenceCost shouldBe 216
						/*case "I0000000000" =>
							gardenPlot.area shouldBe 10
							gardenPlot.fences shouldBe 30
							gardenPlot.fenceCost shouldBe 300
						case "C0000000000" =>
							gardenPlot.area shouldBe 10
							gardenPlot.fences shouldBe 30
							gardenPlot.fenceCost shouldBe 300
						case "F0000000000" =>
							gardenPlot.area shouldBe 10
							gardenPlot.fences shouldBe 30
							gardenPlot.fenceCost shouldBe 300
						case "V0000000000" =>
							gardenPlot.area shouldBe 10
							gardenPlot.fences shouldBe 30
							gardenPlot.fenceCost shouldBe 300
						case "J0000000000" =>
							gardenPlot.area shouldBe 10
							gardenPlot.fences shouldBe 30
							gardenPlot.fenceCost shouldBe 300
						case "E0000000000" =>
							gardenPlot.area shouldBe 10
							gardenPlot.fences shouldBe 30
							gardenPlot.fenceCost shouldBe 300
						case "M0000000000" =>
							gardenPlot.area shouldBe 10
							gardenPlot.fences shouldBe 30
							gardenPlot.fenceCost shouldBe 300
						case "S0000000000" =>
							gardenPlot.area shouldBe 10
							gardenPlot.fences shouldBe 30
							gardenPlot.fenceCost shouldBe 300
						case "I0000000000" =>
							gardenPlot.area shouldBe 10
							gardenPlot.fences shouldBe 30
							gardenPlot.fenceCost shouldBe 300*/
						case _ => ()
				}
				field.fenceCost shouldBe 1930
			}
			"should create field for multiple one-sized plots" in {
				val locatedPlows = createPlowsMap(multipleOneSizedPlotsSampleInput)
				val field = Field(locatedPlows)
				field.plows should have size 25
				field.gardenPlots should have size 5
				field.gardenPlots.foreach { gardenPlot =>
					val xPlotsRegex = "X000000000.".r
					gardenPlot.id match
						case "O0000000000" =>
							gardenPlot.area shouldBe 21
							gardenPlot.fences shouldBe 36
							gardenPlot.fenceCost shouldBe 756
						case xPlotsRegex() =>
							gardenPlot.area shouldBe 1
							gardenPlot.fences shouldBe 4
							gardenPlot.fenceCost shouldBe 4
				}
				field.fenceCost shouldBe 772
			}
		}
		"should create field for subredit X-loop sample" in {
			val locatedPlows = createPlowsMap(subreditXloopSampleInput)
			val field = Field(locatedPlows)
			field.plows should have size 80
			field.gardenPlots should have size 12
			field.gardenPlots.map { gardenPlot =>
				gardenPlot.id match {
					case id if id.startsWith("A") =>
						gardenPlot.area shouldBe 3
						gardenPlot.fences shouldBe 8
						gardenPlot.fenceCost shouldBe 24
					case id if id.startsWith("X") && gardenPlot.area == 32 =>
						gardenPlot.fences shouldBe 58
						gardenPlot.fenceCost shouldBe 1856
					case id if id.startsWith("B") =>
						gardenPlot.area shouldBe 2
						gardenPlot.fences shouldBe 6
						gardenPlot.fenceCost shouldBe 12
					case id if id.startsWith("D") =>
						gardenPlot.area shouldBe 7
						gardenPlot.fences shouldBe 14
						gardenPlot.fenceCost shouldBe 98
					case id if id.startsWith("E") =>
						gardenPlot.area shouldBe 9
						gardenPlot.fences shouldBe 18
						gardenPlot.fenceCost shouldBe 162
					case id if id.startsWith("X") && gardenPlot.area == 4 =>
						gardenPlot.fences shouldBe 8
						gardenPlot.fenceCost shouldBe 32
					case id if id.startsWith("C") && gardenPlot.area == 6 =>
						gardenPlot.fences shouldBe 10
						gardenPlot.fenceCost shouldBe 60
					case id if id.startsWith("C") && gardenPlot.area == 4 =>
						gardenPlot.fences shouldBe 8
						gardenPlot.fenceCost shouldBe 32
					case id if id.startsWith("F") =>
						gardenPlot.area shouldBe 4
						gardenPlot.fences shouldBe 10
						gardenPlot.fenceCost shouldBe 40
					case id if id.startsWith("G") =>
						gardenPlot.area shouldBe 3
						gardenPlot.fences shouldBe 8
						gardenPlot.fenceCost shouldBe 24
					case id if id.startsWith("H") =>
						gardenPlot.area shouldBe 3
						gardenPlot.fences shouldBe 8
						gardenPlot.fenceCost shouldBe 24
					case id if id.startsWith("X") =>
						fail(s"Unexpected 'X' garden plot $gardenPlot")
					case id if id.startsWith("C") =>
						fail(s"Unexpected 'C' garden plot $gardenPlot")
					case id =>
						fail(s"Unexpected #$id garden plot $gardenPlot")
				}
			}
		}
		"should creadt field for small subredit sampl" in {
			val input = """BBBA
						  |BBAA
						  |BBAC
						  |EAAC""".stripMargin.split("\n").toIndexedSeq
			val locatedPlows = createPlowsMap(input)
			val field = Field(locatedPlows)
			field.plows should have size 16
			field.gardenPlots should have size 4
			field.fenceCost shouldBe 184

		}
	}

}
