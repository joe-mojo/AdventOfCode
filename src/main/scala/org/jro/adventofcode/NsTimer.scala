package org.jro.adventofcode

/**
 * @author joe_mojo.
 *         2025/12/03
 */
class NsTimer() {
	private var startTime: Long = System.nanoTime()
	def getStartTime: Long = startTime
	def reset(): Unit = {
		startTime = System.nanoTime()
	}
	def elapsedMillis(): Double = {
		(System.nanoTime() - startTime) / 1000000D
	}
}
