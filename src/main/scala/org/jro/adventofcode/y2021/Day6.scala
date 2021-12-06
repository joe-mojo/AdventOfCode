package org.jro.adventofcode.y2021

import org.jro.adventofcode

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => MMap}

object Day6 {
	val FishArrayLength = 9
	val MaxFishArrayIndex = FishArrayLength - 1
	val ResetFishArrayIndex = 6

	val fishesOrErr: Either[adventofcode.Error, Array[Long]] = adventofcode.getInputData(2021, 6).map { inputData =>
		inputData.source.getLines().map { line =>
			line.split(",").map(_.toInt).foldLeft(MMap.empty[Int, Int]) { (fishMap, fish) =>
				fishMap.put(fish, fishMap.getOrElse(fish, 0) + 1)
				fishMap
			}
		}.reduce(_ ++ _).foldLeft(Array.fill(FishArrayLength)(0L)){ (fishTimers, fishEntry) =>
			fishTimers(fishEntry._1) = fishEntry._2
			fishTimers
		}
	}

	def afterOneDay(fishes: Array[Long]): Array[Long] = {
		((fishes.length - 1)to (0, -1)).foldLeft(Array.fill(FishArrayLength)(0L)) { (newFishArray, index) =>
			if(index > 0) {
				newFishArray(index - 1) = fishes(index)
			} else {
				newFishArray(MaxFishArrayIndex) = fishes(0)
				newFishArray(ResetFishArrayIndex) += fishes(0)
			}
			newFishArray
		}
	}

	def fishToString(fish: Int): String = {
		fish match {
			case 8 => s"\u001B[92m$fish${Console.RESET}"
			case 0 => s"\u001B[91m$fish${Console.RESET}"
			case _ => s"\u001B[93m$fish${Console.RESET}"
		}
	}

	def fishArrayToString(fishes: Array[Long]) = {
		fishes.zipWithIndex.map {
			case (value, index) => (fishToString(index), value)
		}.toSeq.mkString(", ")
	}

	def puzzle(fishes: Array[Long], nbDays: Int): Long = {
		println(s"Initial state: ${fishArrayToString(fishes)} (${fishes.sum} fishes)")
		var currentFishes = fishes
		(1 to nbDays).foreach { day =>
			currentFishes = afterOneDay(currentFishes)
			println(s"After $day: ${fishArrayToString(currentFishes)} ${currentFishes.sum} fishes")
		}
		currentFishes.sum
	}


	def main(args: Array[String]): Unit = {
		// 3,4,3,1,2
		//                     0  1  2  3  4  5  6  7  8
		var testFishes = Array(0L, 1, 1, 2, 1, 0, 0, 0, 0)
		(0 to 18).foreach { d =>
			val fishDisplay = fishArrayToString(testFishes)
			println(s"Day $d: ${fishDisplay} (${testFishes.sum} fishes)") // up to 26
			testFishes = afterOneDay(testFishes)
		}
		println(s"Day 6.1 : ${fishesOrErr.map(fishes => puzzle(fishes.toArray, 80))}") // 345793
		println(s"Day 6.2 : ${fishesOrErr.map(fishes => puzzle(fishes.toArray, 256))}")
	}
}
