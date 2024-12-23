package org.jro.adventofcode.y2024

import org.jro.adventofcode
import org.jro.adventofcode.Error.IOError

import scala.util.Using
import scala.collection.mutable.ListBuffer
/**
 * @author joe_mojo.
 *         2024/12/23
 */
object Day9 {

	sealed trait Cluster {
		def size: Int
	}

	object Cluster {
		case class File(id: Int, override val size: Int) extends Cluster
		case class Free(override val size: Int) extends Cluster

		def parseString(input: String): List[(Cluster.File, Cluster.Free)] = {
			input.sliding(2,  2).zipWithIndex.map {(pair, index) =>
					if(pair.length>1) {
						File(index, pair(0).toInt) -> Free(pair(1).toInt)
					} else {
						File(index, pair(0).toInt) -> Free(0)
					}
			}.toList
		}

		def defragStep(clusters: Seq[(Cluster.File, Cluster.Free)]): Seq[(Cluster.File, Cluster.Free)] = {
			// 1. Find the firstlarge enough free cluster
			val clusterToMove: (File, Free) = clusters.last
			// start of Copilot gen ----- TODO Check if it's correct -----
			clusters.collectFirst((file, free) => free.size >= clusterToMove._2.size) match {
				case Some((file, free)) =>
					// 2. Move the file to the free cluster
					val newClusters = clusters.updated(clusters.indexOf((file, free)), (file, Free(free.size - clusterToMove._2.size)))
					newClusters.updated(newClusters.indexOf((clusterToMove._1, Free(0))), (clusterToMove._1, Free(clusterToMove._2.size)))
				case None =>
					clusters
			}
			// end of Copilot gen ----------------------------------------
			//clusters.

			???
		}
	}



	def puzzle1(input: String): Int = {
		val inputTopology: Seq[(Cluster.File, Cluster.Free)] = Cluster.parseString(input)

		0
	}

	def main(args: Array[String]): Unit = {
		adventofcode.getInputData(2024, 9).flatMap { inputData =>
			Using(inputData.source) { source =>
				source.toString
			}.toEither.left.map(throwable => IOError(inputData.resource, throwable))
		} match
			case Right(inputString) =>
				println(s"Puzzle 1 = ${puzzle1(inputString)}")
				//println(s"Puzzle 2 = ${puzzle2(initialField)}")
			case Left(err) =>
				println(s"Puzzle input didn't load ! Reason:\n $err")
	}
}
