package org.jro.adventofcode.y2021

import org.jro.adventofcode
import org.jro.adventofcode.y2021.Day3.Bit.{One, Zero}

import scala.annotation.tailrec

object Day3  extends App {
	case class BitCount(zeroes: Int = 0, ones: Int = 0) {
		def count(b: Bit):BitCount = {
			b match {
				case Zero => this.copy(zeroes = this.zeroes + 1)
				case One => this.copy(ones = this.ones + 1)
			}
		}
	}
	object BitCount {
		def initSeq(bitCount: Int): Seq[BitCount] = {
			Seq.fill(bitCount)(BitCount())
		}
		def puzzleOneMostCommon(count: BitCount): Bit = if(count.zeroes > count.ones) Zero else One
		def o2genMostCommon(count: BitCount): Bit = puzzleOneMostCommon(count)
		def co2scrubLeastCommon(count: BitCount): Bit = if(count.zeroes <= count.ones) Zero else One
	}

	sealed trait Bit {
		def flip: Bit
	}
	object Bit {
		case object Zero extends Bit {
			override val flip: Bit = One
		}
		case object One extends Bit {
			override val flip: Bit = Zero
		}
		def fromChar(bit: Char): Bit = {
			bit match {
				case '0' => Zero
				case '1' => One
			}
		}
		def toInt(bits: Seq[Bit]): Int = {
			bits.reverse.zipWithIndex.map {
				case (Zero, index) => 0
				case (One, index) => 1 << index
			}.reduce { (shifted1, shifted2) =>
				shifted1 | shifted2
			}
		}
		def flip(bits: Seq[Bit]): Seq[Bit] = bits.map(_.flip)
	}
	type BitSeq = IndexedSeq[Bit]

	def parseLine(binaryString: String): IndexedSeq[Bit] = {
		binaryString.map(Bit.fromChar)
	}

	def renderNumber(n: Int): String = {
		s"$n (${Integer.toString(n, 2)})"
	}

	def puzzle1Res: Either[adventofcode.Error, Int] = {
		adventofcode.getInputSourceOf(2021, 3).map { source =>
			source.getLines().map(parseLine).foldLeft(BitCount.initSeq(12)){ (bitCounts, binaryNumber) =>
				binaryNumber.zip(bitCounts).map {
					case (One, BitCount(z,o)) => BitCount(z, o + 1)
					case (Zero, BitCount(z, o)) => BitCount(z + 1, o)
				}
			}
		}.map { bitCounts =>
			println(s"Bitcounts: ${bitCounts}")
			val bits = bitCounts.map(BitCount.puzzleOneMostCommon(_))
			println(s"Most common bits = ${bits}")
			val gamma = Bit.toInt(bits)
			val epsilon = Bit.toInt(Bit.flip(bits))
			println(s"gamma = ${renderNumber(gamma)}; epsilon = ${renderNumber(epsilon)}")
			gamma * epsilon
		}
	}

	def findBit(binNumSeq: Seq[BitSeq], bitIndex: Int, bitCriteria: BitCount => Bit): Bit = {
		bitCriteria(binNumSeq.foldLeft(BitCount()){(bitCount, binaryNumber) =>
			bitCount.count(binaryNumber(bitIndex))
		})
	}

	val findO2MostCommonBit: (Seq[BitSeq], Int) => Bit = (binNum, idx) => findBit(binNum, idx, BitCount.o2genMostCommon)
	val findCO2LeastCommonBit: (Seq[BitSeq], Int) => Bit = (binNum, idx) => findBit(binNum, idx, BitCount.co2scrubLeastCommon)


	def findRating(binNumSeq: Seq[BitSeq], criteria: (Seq[BitSeq], Int) => Bit): BitSeq = {
		@tailrec
		def filterNums(remainingBinNums: Seq[BitSeq], bitIndex: Int, maxBitIndex: Int): BitSeq = {
			val selectedBit: Bit = criteria(remainingBinNums, bitIndex)
			val filtered = remainingBinNums.filter { binNum =>
				binNum(bitIndex) == selectedBit
			}
			if(filtered.length > 1 && bitIndex <= maxBitIndex) filterNums(filtered, bitIndex + 1, maxBitIndex)
			else filtered.head
		}
		filterNums(binNumSeq, 0, binNumSeq.head.length - 1)
	}

	def puzzle2Res: Either[adventofcode.Error, Int] = {
		adventofcode.getInputSourceOf(2021, 3).map { source =>
			val allBinNums: Seq[BitSeq] = source.getLines().map(parseLine).toSeq
			val o2rating: Int = Bit.toInt(findRating(allBinNums, findO2MostCommonBit))
			val co2rating: Int = Bit.toInt(findRating(allBinNums, findCO2LeastCommonBit))
			println(s"O2 rating = ${renderNumber(o2rating)}")
			println(s"CO2 rating = ${renderNumber(o2rating)}")
			o2rating * co2rating
		}
	}


	println(s"Day 3.1 - power consumption = $puzzle1Res")
	println(s"Day 3.2 - life support rating = $puzzle2Res")

}
