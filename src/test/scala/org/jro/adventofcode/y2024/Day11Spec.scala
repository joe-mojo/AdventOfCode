package org.jro.adventofcode.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*

/**
 * @author joe_mojo.
 *         2025/01/11
 */
class Day11Spec extends AnyFreeSpec with Matchers {
	"blink" - {
		"with 0 1 10 99 999" - {
			val stoneLine = List(0L, 1L, 10L, 99L, 999L)
			"should blink the stones" in {
				Day11.blink(stoneLine) shouldBe List(1L, 2024L, 1L, 0L, 9L, 9L, 2021976L)
			}
		}
		"with 125 17" - {
			val stoneLine = List(125L, 17L)
			"should blink the stones 6 times" in {
				val expectedAfterPass1 = Day11.parseInput("253000 1 7")
				val expectedAfterPass2 = Day11.parseInput("253 0 2024 14168")
				val expectedAfterPass3 = Day11.parseInput("512072 1 20 24 28676032")
				val expectedAfterPass4 = Day11.parseInput("512 72 2024 2 0 2 4 2867 6032")
				val expectedAfterPass5 = Day11.parseInput("1036288 7 2 20 24 4048 1 4048 8096 28 67 60 32")
				val expectedAfterPass6 = Day11.parseInput("2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2")
				Day11.blink(stoneLine) shouldBe expectedAfterPass1
				Day11.blink(expectedAfterPass1) shouldBe expectedAfterPass2
				Day11.blink(expectedAfterPass2) shouldBe expectedAfterPass3
				Day11.blink(expectedAfterPass3) shouldBe expectedAfterPass4
				Day11.blink(expectedAfterPass4) shouldBe expectedAfterPass5
				Day11.blink(expectedAfterPass5) shouldBe expectedAfterPass6
			}
			"should blink the stones 25 times and get 55312 stones" in {
				(1 to 25).foldLeft(stoneLine) { (stones, _) =>
					Day11.blink(stones)
				}.size shouldBe 55312
			}
		}

	}

	"blinkDeepAsync" - {
		implicit val executorService: ThreadPoolExecutor = new ThreadPoolExecutor(
			4, 4, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue[Runnable]()
		)
		implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executorService)
		"with 0 1 10 99 999" - {
			val stoneLine = List(0L, 1L, 10L, 99L, 999L)
			"should blink the stones" in {
				Day11.blinkDeepPar(stoneLine, 1) shouldBe 7
			}
		}
		"with 125 17" - {
			val stoneLine = List(125L, 17L)
			"should blink the stones 25 times and get 55312 stones" in {
				Day11.blinkDeepPar(stoneLine, 25) shouldBe 55312
			}
		}
	}
	"blinkDeepSync" - {
		"with 0 1 10 99 999" - {
			val stoneLine = List(0L, 1L, 10L, 99L, 999L)
			"should blink the stones" in {
				Day11.blinkDeepSync(stoneLine, 1) shouldBe 7
			}
		}
		"with 125 17" - {
			val stoneLine = List(125L, 17L)
			"should blink the stones 25 times and get 55312 stones" in {
				Day11.blinkDeepSync(stoneLine, 25) shouldBe 55312
			}
		}
	}
}
