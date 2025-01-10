package org.jro.adventofcode.y2024

import org.jro.adventofcode
import org.jro.adventofcode.Error.IOError

import scala.annotation.tailrec
import scala.util.Using
import scala.collection.mutable.{ListBuffer, SortedMap => MutableSortedMap}
/**
 * @author joe_mojo.
 *         2024/12/23
 */
object Day9 {

	object Puzzle1 {
		case class Disk(clusters: ListBuffer[Cluster], capacity: Int, totalUsed: Int, totalFree: Int) {
			def checksum: Long = {
				(clusters.foldLeft((0, 0L)) {
					case ((index, checksum), cluster) =>
						(
								index + cluster.file.blockCount + cluster.freeSpaceAfter,
								checksum + (index until (cluster.file.blockCount + index)).map(idx => idx.toLong * cluster.file.fileId).sum
						)
				})._2
			}

			def defragCompact: Disk = {
				def moveBlocks(workingClusters: ListBuffer[Cluster]): ListBuffer[Cluster] = {
					val firstFreeIndex = workingClusters.indexWhere(_.freeSpaceAfter > 0)
					if (firstFreeIndex != workingClusters.length - 1) {
						val clusterWithFreeSpace = workingClusters.remove(firstFreeIndex)
						val clusterWithBlocksToMove = workingClusters.last
						workingClusters.remove(workingClusters.length - 1)
						val blocksToMove = Math.min(clusterWithBlocksToMove.file.blockCount, clusterWithFreeSpace.freeSpaceAfter)
						workingClusters.insert(firstFreeIndex, Cluster(clusterWithFreeSpace.file, 0))
						workingClusters.insert(firstFreeIndex + 1, Cluster(FileBlocks(clusterWithBlocksToMove.file.fileId, blocksToMove), clusterWithFreeSpace.freeSpaceAfter - blocksToMove))
						if (blocksToMove < clusterWithBlocksToMove.file.blockCount)
							workingClusters.append(Cluster(FileBlocks(clusterWithBlocksToMove.file.fileId, clusterWithBlocksToMove.file.blockCount - blocksToMove), clusterWithBlocksToMove.freeSpaceAfter + blocksToMove))
						else {
							val newLastOne = workingClusters.last
							workingClusters.remove(workingClusters.length - 1)
							workingClusters.append(Cluster(newLastOne.file, newLastOne.freeSpaceAfter + clusterWithBlocksToMove.size))
						}
					}
					workingClusters
				}

				@tailrec
				def defragStep(disk: Disk): Disk = {
					if (disk.clusters.last.freeSpaceAfter == disk.totalFree) disk
					else {
						defragStep(Disk(moveBlocks(disk.clusters), disk.capacity, disk.totalUsed, disk.totalFree))
					}
				}

				defragStep(this)
			}

		}

		object Disk {
			def from(cluster: Seq[Cluster]): Disk = {
				val (totalUsed, totalFree) = cluster.foldLeft((0, 0)) { case ((used, free), cluster) =>
					(used + cluster.file.blockCount, free + cluster.freeSpaceAfter)
				}
				Disk(ListBuffer.from(cluster), totalUsed + totalFree, totalUsed, totalFree)
			}
		}

		case class Cluster(file: FileBlocks, freeSpaceAfter: Int) {
			def size: Int = file.blockCount + freeSpaceAfter
		}

		case class FileBlocks(fileId: Int, val blockCount: Int)

		object Cluster {

			def parseString(input: String): List[Cluster] = {
				input.sliding(2, 2).zipWithIndex.map { (pair, index) =>
					if (pair.length > 1) {
						Cluster(FileBlocks(index, pair.substring(0, 1).toInt), pair.substring(1).toInt)
					} else {
						Cluster(FileBlocks(index, pair.toInt), 0)
					}
				}.toList
			}
		}

		def apply(input: String): Long = {
			val inputTopology: Seq[Cluster] = Cluster.parseString(input)
			val disk = Disk.from(inputTopology)
			println(s"Puzzle1.Disk capacity = ${disk.capacity}; total used = ${disk.totalUsed}; total free = ${disk.totalFree}")
			disk.defragCompact.checksum
		}
	}

	object Puzzle2 {
		case class File(id: Int, offset: Int, size: Int) {
			override def toString: String = s"File(#${id}@${offset}:$size)"
		}
		case class DetachedBlocks(fileId: Int, blockCount: Int, freeSpaceAfter: Int)

		case class Cluster(file: File, freeSpaceAfter: Int) {
			def size: Int = file.size + freeSpaceAfter
		}
		object Cluster {
			def parseString(input: String): MutableSortedMap[Int, Cluster] = {
				MutableSortedMap.from(input.sliding(2, 2).zipWithIndex.map { (pair, fileId) =>
					if (pair.length > 1) {
						DetachedBlocks(fileId, pair.substring(0, 1).toInt, pair.substring(1).toInt)
					} else {
						DetachedBlocks(fileId, pair.toInt, 0)
					}
				}.foldLeft((0, Seq.empty[Cluster])) {
					case ((offset, clusters), DetachedBlocks(fileId, blockCount, freeSpaceAfter)) =>
						(offset + blockCount + freeSpaceAfter, clusters :+ Cluster(File(fileId, offset, blockCount), freeSpaceAfter))
				}._2.map(cluster => cluster.file.id -> cluster))
			}
		}

		case class Disk(clusters: MutableSortedMap[Int, Cluster], capacity: Int, totalUsed: Int, totalFree: Int) {
			def checksum: Long = {
				clusters.foldLeft(0L) {
					case (checksum, (fileId, cluster)) =>
						checksum + (cluster.file.offset until (cluster.file.offset + cluster.file.size)).map(offset => offset.toLong * fileId).sum

				}
			}

			private def findFirstFreeCluster(neededSpace: Int): Option[Cluster] = clusters.values.toList.sortBy(_.file.offset).find(_.freeSpaceAfter >= neededSpace)

			def findLastCluster: Cluster = clusters.maxBy(_._2.file.offset)._2

			private def findLastClusterBefore(cluster: Cluster): Option[Cluster] = clusters.filter(_._2.file.offset < cluster.file.offset).values.maxByOption(_.file.offset)

			def move(dest: Cluster, src: Cluster, maybeBeforeSrc: Option[Cluster]): Boolean = {
				if(dest.freeSpaceAfter >= src.file.size) {
					val newDest = Cluster(dest.file, 0)
					val newSrc = Cluster(src.file.copy(offset = newDest.file.offset + newDest.size), dest.freeSpaceAfter - src.file.size)

					clusters.update(dest.file.id, newDest)
					clusters.update(src.file.id, newSrc)
					maybeBeforeSrc.foreach { beforeSrc =>
						clusters.update(beforeSrc.file.id, Cluster(beforeSrc.file, beforeSrc.freeSpaceAfter + src.size))
					}
					true
				} else false
			}

			def defragByFile: Disk = {
				@tailrec
				 def defragStep(disk: Disk, fileIdsQueue: List[Int]): Disk = {
					if(fileIdsQueue.isEmpty) disk
					else {
						val lastCluster: Cluster = disk.clusters(fileIdsQueue.head)
						disk.findFirstFreeCluster(lastCluster.file.size) match {
							case Some(freeCluster) if freeCluster.file.offset >= lastCluster.file.offset =>
								defragStep(disk, fileIdsQueue.tail)
							case Some(freeCluster) =>
								disk.move(freeCluster, lastCluster, findLastClusterBefore(lastCluster))
								defragStep(disk, fileIdsQueue.tail)
							case None =>
								defragStep(disk, fileIdsQueue.tail)
						}
					}
				}
				defragStep(this, this.clusters.keys.tail.toList.reverse)
			}
		}

		object Disk {
			def from(clusters: MutableSortedMap[Int, Cluster]): Disk = {
				val (totalUsed, totalFree) = clusters.foldLeft((0, 0)) { case ((used, free), (_, cluster)) =>
					(used + cluster.file.size, free + cluster.freeSpaceAfter)
				}
				Disk(clusters, totalUsed + totalFree, totalUsed, totalFree)
			}
		}


		def apply(input: String): Long = {
			val inputTopology: MutableSortedMap[Int, Cluster] = Cluster.parseString(input)
			val disk = Disk.from(inputTopology)
			println(s"Puzzle2.Disk capacity = ${disk.capacity}; total used = ${disk.totalUsed}; total free = ${disk.totalFree}")
			println(s"Puzzle2.Disk before defrag checksum = ${disk.checksum}")
			disk.defragByFile.checksum
		}
	}




	def main(args: Array[String]): Unit = {
		adventofcode.getInputData(2024, 9).flatMap { inputData =>
			Using(inputData.source) { source =>
				source.mkString.trim
			}.toEither.left.map(throwable => IOError(inputData.resource, throwable))
		} match
			case Right(inputString) =>
				println(s"Puzzle 1 = ${Puzzle1(inputString)}") // 6337367222422 OK
				println(s"Puzzle 2 = ${Puzzle2(inputString)}") // 6361380647183 OK, slow TODO: optimize by ordering by offset ?
			case Left(err) =>
				println(s"Puzzle input didn't load ! Reason:\n $err")
	}
}
