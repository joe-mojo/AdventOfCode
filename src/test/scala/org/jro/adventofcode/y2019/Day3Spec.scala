package org.jro.adventofcode.y2019

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class Day3Spec extends AnyFreeSpec with Matchers {
	val points = List(Point2(-610,675), Point2(-517,777), Point2(-274,1105), Point2(-886,557), Point2(-771,675), Point2(-274,1269), Point2(-999,557), Point2(-274,1286), Point2(-309,1411), Point2(-557,1201), Point2(-771,987), Point2(-490,1269), Point2(-1648,112), Point2(-490,1286), Point2(-517,1269), Point2(-517,1286), Point2(-1728,-115), Point2(-1320,536), Point2(-1753,112), Point2(-1470,536), Point2(-1753,282), Point2(-751,1286), Point2(-1648,413), Point2(-905,1201), Point2(-905,1258), Point2(-905,1267), Point2(-766,1509), Point2(-766,1526), Point2(-899,1506), Point2(-1444,1051), Point2(-2674,-53), Point2(-714,2037), Point2(-2674,-237), Point2(-648,2290), Point2(-714,2290), Point2(-3076,-53), Point2(-3110,-53), Point2(-880,2290), Point2(-3429,-97), Point2(-3429,-198), Point2(-1944,1899), Point2(-3857,-97), Point2(-3857,-198), Point2(-2028,2031), Point2(-2501,1686), Point2(-2501,1786), Point2(-2170,2131), Point2(-2744,1680), Point2(-2334,2131), Point2(-2345,2131), Point2(-2501,2031), Point2(-3882,1595))
	val manathanDistanceToOrigin: Point2 => Int =  Day3.manathanDistance(Point2(0, 0))

	"Manhatan distance" - {
		"of origin and Point2(-610,675)" - {
			"should be 1285" in {
				manathanDistanceToOrigin(Point2(-610, 675)) shouldBe 1285
			}
		}
		"of origin and Point2(-517,777)" - {
			"should be 1294" in {
				manathanDistanceToOrigin(Point2(-517, 777)) shouldBe 1294
			}
		}
		"to origin for points inRules list of crossing points" - {
			"should be growing" in {
				for {
					pointCouple <- points.sliding(2, 1)
				} {
					manathanDistanceToOrigin(pointCouple.head) should be <= manathanDistanceToOrigin(pointCouple.last)
				}
			}
		}
	}

	val example1 = ("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")
	"parseLine" - {
		"should the right vectors" in {
			Day3.parseLine(example1._1) should contain theSameElementsInOrderAs  Seq(
				/*R75*/AnchoredVector2(Point2(0,0), Vector2(75, 0)),
				/*D30*/AnchoredVector2(Point2(75, 0), Vector2(0, -30)),
				/*R83*/AnchoredVector2(Point2(75, -30), Vector2(83, 0)),
				/*U83*/AnchoredVector2(Point2(158, -30), Vector2(0, 83)),
				/*L12*/AnchoredVector2(Point2(158, 53), Vector2(-12, 0)),
				/*D49*/AnchoredVector2(Point2(146, 53), Vector2(0,-49)),
				/*R71*/AnchoredVector2(Point2(146, 4), Vector2(71, 0)),
				/*U7*/AnchoredVector2(Point2(217, 4), Vector2(0, 7)),
				/*L72*/AnchoredVector2(Point2(217, 11), Vector2(-72, 0))
			)
		}
	}

	"AncoredVector when" - {
		"rendering points" - {
			"should give all points belonging to the vector" in {
				val points = AnchoredVector2(Point2(0,0), Vector2(75, 0)).raster
				points should have length 76
				points.head shouldBe Point2(0, 0)
				points.last shouldBe Point2(75, 0)

				val points2 = AnchoredVector2(Point2(217, 4), Vector2(0, 7)).raster
				points2 should have length 8
				points2 should contain theSameElementsInOrderAs Seq(Point2(217, 4), Point2(217, 5), Point2(217, 6), Point2(217, 7), Point2(217, 8), Point2(217, 9), Point2(217, 10), Point2(217, 11))
			}
		}
	}

	"findCrossingPoints" - {
		"should find identical points beween 2 wires" in {
			val wire1 = Day3.parseLine(example1._1)
			val wire2 = Day3.parseLine(example1._2)

			val wire1points = Day3.rasterWire(wire1)
			val wire2points = Day3.rasterWire(wire2)

			println(s"wire1point = $wire1points")
			println(s"wire2point = $wire2points")
		}
	}

	"stepsToIntersection" - {

		def checkLowestCost(input: (String, String), expected: Int): Unit = {
			val wires = (Day3.parseLine(input._1), Day3.parseLine(input._2))
			val rastW1 = Day3.rasterWire(wires._1)
			val rastW2 = Day3.rasterWire(wires._2)
			val crossPts: Seq[Point2] = Day3.findCrossingPoints(wires._1, wires._2)

			val sortedCrossPts =  crossPts.flatMap(Day3.stepsToIntersection(rastW1, rastW2)).sorted
			sortedCrossPts.head shouldBe expected
		}
		"should find the number of combined steps to reach an intersection" in {
			checkLowestCost("R75,D30,R83,U83,L12,D49,R71,U7,L72" -> "U62,R66,U55,R34,D71,R55,D58,R83", 610)
			checkLowestCost("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" -> "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 410)

		}
	}
}
