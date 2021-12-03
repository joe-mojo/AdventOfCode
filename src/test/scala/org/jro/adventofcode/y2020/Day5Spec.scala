package org.jro.adventofcode.y2020

import org.jro.adventofcode.y2020.Day5.Seat
import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day5Spec extends AnyFreeSpec with Matchers with Inside{
	val sampleInput = Seq(
		"FBFBBFFRLR", //row 44, column 5, seat ID 357
		"BFFFBBFRRR", //row 70, column 7, seat ID 567
		"FFFBBBFRRR", //row 14, column 7, seat ID 119
		"BBFFBBFRLL"  //row 102, column 4, seat ID 820
	)

	val expectedSeats = Seq(
		Seat(44.toByte, 5.toByte),
		Seat(70.toByte, 7.toByte),
		Seat(14.toByte, 7.toByte),
		Seat(102.toByte, 4.toByte)
	)

	val expectedIds = Seq(
		357,
		567,
		119,
		820
	)

	"Seat" - {
		"should be parsed correctly" in {
			for (i <- sampleInput.indices) {
				val actualMaybeSeat = Seat.fromPartionString(sampleInput(i))
				val expectedSeat = expectedSeats(i)
				actualMaybeSeat shouldNot be(Symbol("empty"))
				actualMaybeSeat.map { actualSeat =>
					actualSeat shouldBe expectedSeat
					actualSeat.id shouldBe expectedIds(i)
				}
			}
		}
	}

}
