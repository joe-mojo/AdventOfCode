package org.jro.adventofcode.y2024

import java.util.concurrent.{ExecutorService, Executors, LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.collection.mutable.{Map => MutableMap}

/**
 * @author joe_mojo.
 *         2025/01/11
 */
object Day11 {
	val Input: String = "7725 185 2 132869 0 1840437 62 26310"
	val InitialStoneLine: List[Long] = parseInput(Input)
	private val Zero = 0L
	private val One = 1L
	private val ListOfOne = List(One)
	def parseInput(input: String): List[Long] = input.split(" ").map(_.toLong).toList

	private def numberOfDigits(n: Long): Int = {
		if (n == 0) 1 else Math.log10(Math.abs(n)).toInt + 1
	}

	private def splitNumber(n: Long): List[Long] = {
		val s = n.toString
		val halfLen = s.length / 2
		List(
			s.substring(0, halfLen).toLong,
			s.substring(halfLen).toLong
		)
	}

	def blink(stoneLine: List[Long]): List[Long] = {
		stoneLine.flatMap { stone =>
			if(stone == Zero) {
				ListOfOne
			} else if(numberOfDigits(stone) %2 == 0){
				splitNumber(stone)
			} else {
				List(stone * 2024)
			}
		}
	}

	class Chrono() {
		var start: Long = System.currentTimeMillis()
		def reset(): Unit = {
			start = System.currentTimeMillis()
		}
		def elapsed(): Long = System.currentTimeMillis() - start
	}

	private val cache = MutableMap.empty[(Int, Long), Long] // (depth, stone) -> count

	def blinkDeepPar(stoneLine: List[Long], initialCount: Int)(implicit ec: ExecutionContext, es: ThreadPoolExecutor): Long = {
		def blinkDeepRec(stoneLine: List[Long], counter: Int)(implicit ec: ExecutionContext): Future[Long] = {
			if(counter == 0) {
				Future.successful(stoneLine.size)
			} else {
				val futures = stoneLine.map { stone =>
						val cached: Option[Long] = cache.get((counter, stone))
						if(cached.isDefined) {
							Future.successful(cached.get)
						} else {
							Future {
								blinkDeepRec(blink(List(stone)), counter - 1)
							}.flatten
						}
				}
				Future.sequence(futures).map(_.sum)
			}
		}
		Await.result(blinkDeepRec(stoneLine, initialCount), 5.minutes)
	}

	def blinkDeepSync(stoneLine: List[Long], initialCount: Int): Long = {
		def blinkDeepRec(stoneLine: List[Long], counter: Int): Long = {
			if (counter == 0) {
				stoneLine.size
			} else {
				stoneLine.map { stone =>
					cache.getOrElseUpdate((counter, stone), {
						blinkDeepRec(blink(List(stone)), counter - 1)
					})
				}.sum
			}
		}

		blinkDeepRec(stoneLine, initialCount)
	}

	def puzzle1(): Long = {
		(1 to 25).foldLeft(InitialStoneLine) { (stones, _) =>
			blink(stones)
		}.size
	}

	private def puzzle2Par(): Long = {
		val threadCount = Runtime.getRuntime.availableProcessors
		implicit val executorService: ThreadPoolExecutor = new ThreadPoolExecutor(
			threadCount, threadCount * 2, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue[Runnable]()
		)
		implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executorService)
		try {
			blinkDeepPar(InitialStoneLine, 75)
		} finally {
			executorService.shutdown()
		}
	}

	private def puzzle2Sync(): Long = {
		blinkDeepSync(InitialStoneLine, 75)
	}

	def main(args: Array[String]): Unit = {
		val chrono = new Chrono()
		println(s"Puzzle1: ${puzzle1()}") // 233050 OK
		println(s"\tElapsed time: ${chrono.elapsed()} ms")
		chrono.reset()
		println(s"Puzzle2 sync: ${puzzle2Sync()}") // 276661131175807 OK
		println(s"\tElapsed time: ${chrono.elapsed()} ms")
		chrono.reset()
		println(s"Puzzle2 async with parallelization: ${puzzle2Par()}") // 276661131175807 OK
		println(s"\tElapsed time: ${chrono.elapsed()} ms")
	}
}
