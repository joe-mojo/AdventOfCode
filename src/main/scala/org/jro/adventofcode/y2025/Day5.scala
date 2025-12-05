package org.jro.adventofcode.y2025

import org.jro.adventofcode
import org.jro.adventofcode.Error
import org.jro.adventofcode.Error._
import org.jro.adventofcode.InputData

import scala.collection.immutable.NumericRange
import scala.util.Try

/**
 * @author joe_mojo.
 *         2025/12/05
 */
object Day5 {

	private[y2025] class ProductsData(ranges: Seq[NumericRange[Long]], val productIds: Seq[Long]) {
		val sortedFreshRanges: Seq[NumericRange[Long]] = ProductsData.mergeRanges(ranges)

		def isProductFresh(productId: Long): Boolean = {
			ranges.exists(range => productId>= range.start && productId <= range.end)
		}

		def countFreshProductsInRanges: Long = {
			sortedFreshRanges.map(range => range.end - range.start + 1).sum
		}
	}
	private[y2025] object ProductsData {
		def apply(ranges: Seq[NumericRange[Long]], productIds: Seq[Long]): ProductsData = {
			new ProductsData(ranges, productIds)
		}
		private[y2025] def mergeRanges(ranges: Seq[NumericRange[Long]]): Seq[NumericRange[Long]] = {
			if (ranges.isEmpty) return Seq.empty

			val sortedRanges = ranges.sortBy(_.start)
			val mergedRanges = scala.collection.mutable.ListBuffer[NumericRange[Long]]()
			var currentRange = sortedRanges.head

			for (nextRange <- sortedRanges.tail) {
				if (currentRange.end >= nextRange.start) {
					currentRange = currentRange.start to Math.max(currentRange.end, nextRange.end)
				} else {
					mergedRanges += currentRange
					currentRange = nextRange
				}
			}
			mergedRanges += currentRange

			mergedRanges.toSeq
		}
	}

	private[y2025] def parseRange(rangeLine: String): Either[Error, NumericRange[Long]] = {
		sequence(rangeLine.split('-').map(numStr => Try(numStr.toLong).toEither.left.map(_ => NaN(numStr)))).flatMap {
			case Seq(start, end) if start <= end => Right(start to end)
			case Seq(start, end) => Left(UnexpectedValue("Range end", s">= $start", s"$end"))
			case _ => Left(WrongFixedSplit(rangeLine, "-".r, 2, rangeLine.split('-').length))
		}
	}
	private[y2025] def unsafeParseRange(rangeLine: String): NumericRange[Long] = {
		val Array(start, end) = rangeLine.split('-').map(_.toLong)
		start to end
	}

	private[y2025] def parseInput(input: InputData): Either[Error, ProductsData] = {

		val lineIterator = input.source.getLines()
		for {
			ranges <- sequence(lineIterator.takeWhile(line => line.nonEmpty).map(parseRange))
			productIds <- sequence(lineIterator.map(line => Try(line.toLong).toEither.left.map(_ => NaN(line))))
		} yield ProductsData(ranges, productIds)
	}

	def puzzle1(input: ProductsData): Long = {
		input.productIds.count(input.isProductFresh)
	}

	def puzzle2(input: ProductsData): Long = {
		input.countFreshProductsInRanges
	}

	def main(args: Array[String]): Unit = adventofcode.y2025.mainWithTransformer(5, puzzle1 /* OK: 598*/, puzzle2 /* OK: 360341832208407 */, parseInput)
}
