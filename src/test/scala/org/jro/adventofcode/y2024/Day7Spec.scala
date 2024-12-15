package org.jro.adventofcode.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

/**
 * @author joe_mojo.
 *         2024/12/15
 */
class Day7Spec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {
	import Day7._
	"Day7" - {
		"with sample input" - {
			// 190 has 1 solution, 3267 has 2 solutions and 292 has one.
			val sampleInput = """190: 10 19
								|3267: 81 40 27
								|83: 17 5
								|156: 15 6
								|7290: 6 8 6 15
								|161011: 16 10 13
								|192: 17 8 14
								|21037: 9 7 18 13
								|292: 11 6 16 20""".stripMargin.split("\n").map(parseLine).toSeq

			"should give 3749 as puzzle1 answer" in {
				puzzle1(sampleInput) should be(3749)
			}

			"calibrationLookup1" - {
				"should return 1 full calibration for 190" in {
					calibrationLookup(sampleInput.head, generateExpressions1) should contain theSameElementsAs(List(
						FullCalibration(190, OperationExpression(10, Seq(OperationElement(Operator.`*`, 19))))
					))
				}
				"should return 2 full calibrations for 3267" in {
					calibrationLookup(sampleInput(1), generateExpressions1) should contain theSameElementsAs(List(
						FullCalibration(3267, OperationExpression(81, Seq(OperationElement(Operator.`+`, 40), OperationElement(Operator.`*`, 27)))),
						FullCalibration(3267, OperationExpression(81, Seq(OperationElement(Operator.`*`, 40), OperationElement(Operator.`+`, 27))))
					))
				}
				"should return 1 full calibration for 292" in {
					calibrationLookup(sampleInput(8), generateExpressions1) should contain theSameElementsAs(List(
						FullCalibration(292, OperationExpression(11, Seq(OperationElement(Operator.`+`, 6), OperationElement(Operator.`*`, 16), OperationElement(Operator.`+`, 20))))
					))
				}
				val zeroCalibrationsTable = Table(
					("inputIndex", "description"),
					(2, "83"),
					(3, "156"),
					(4, "7290"),
					(5, "161011"),
					(6, "192"),
					(7, "21037")
				)
				"should return 0 full calibrations1 for" in {
					forAll(zeroCalibrationsTable) { (inputIndex, description) =>
						calibrationLookup(sampleInput(inputIndex), generateExpressions1) should be(empty)
					}
				}

				"should return 0 full calibrations1 for 83" in {
					calibrationLookup(sampleInput(2), generateExpressions1) should be(empty)
				}
				"should return 0 full calibrations1 for 156" in {
					calibrationLookup(sampleInput(3), generateExpressions1) should be(empty)
				}
				"should return 0 full calibrations1 for 7290" in {
					calibrationLookup(sampleInput(4), generateExpressions1) should be(empty)
				}
			}
		}

		"genrateExpressions1" - {
			"should generate 2 expressions" in {
				val calibration = parseLine("190: 10 19")
				generateExpressions1(calibration) should have size 2
			}
			"should generate 16 expressions" in {
				val calibration = parseLine("7290: 6 8 6 15")
				generateExpressions1(calibration) should have size 8
			}
		}

		"OperationExpression" - {
			"should calculate 10 + 19" in {
				val operation = OperationExpression(10, Seq(OperationElement(Operator.`+`, 19)))
				operation.apply should be(29)
			}
			"should calculate 10 * 19" in {
				val operation = OperationExpression(10, Seq(OperationElement(Operator.`*`, 19)))
				operation.apply should be(190)
			}
			"should calculate 81 + 40 * 27 from left to right" in {
				val operation = OperationExpression(81, Seq(OperationElement(Operator.`+`, 40), OperationElement(Operator.`*`, 27)))
				operation.apply should be(3267)
			}
			"should calculate 81 * 40 + 27 from left to right" in {
				val operation = OperationExpression(81, Seq(OperationElement(Operator.`+`, 40), OperationElement(Operator.`*`, 27)))
				operation.apply should be(3267)
			}
			"should calculate 81 + 40 + 27 from left to right" in {
				val operation = OperationExpression(81, Seq(OperationElement(Operator.`+`, 40), OperationElement(Operator.`+`, 27)))
				operation.apply should be(148)
			}
			"should calculate 81 * 40 * 27 from left to right" in {
				val operation = OperationExpression(81, Seq(OperationElement(Operator.`*`, 40), OperationElement(Operator.`*`, 27)))
				operation.apply should be(87480)
			}
			"should calculate 11 + 6 * 16 + 20 from left to right" in {
				val operation = OperationExpression(11, Seq(OperationElement(Operator.`+`, 6), OperationElement(Operator.`*`, 16), OperationElement(Operator.`+`, 20)))
				operation.apply should be(292)
			}
			"should calculate 11 + 6 + 16 * 20 from left to right" in {
				val operation = OperationExpression(11, Seq(OperationElement(Operator.`+`, 6), OperationElement(Operator.`+`, 16), OperationElement(Operator.`*`, 20)))
				operation.apply should be(660)
			}
		}
	}
}
