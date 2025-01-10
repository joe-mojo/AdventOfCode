package org.jro.adventofcode.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/**
 * @author joe_mojo.
 *         2025/01/10
 */
class Day9Spec extends AnyFreeSpec with Matchers {
	val sampleInput = "2333133121414131402"

	"Puzzle1.Disk" - {
		import Day9.Puzzle1._
		"for sample input" - {
			"should report correct capacity, total used and total free" in {
				val disk = Disk.from(Cluster.parseString(sampleInput))
				disk.capacity shouldBe 42
				disk.totalUsed shouldBe 28
				disk.totalFree shouldBe 14
			}
			"should report correct checksum before defrag" in {
				val disk = Disk.from(Cluster.parseString(sampleInput))
				disk.checksum shouldBe 4116
			}
			"should report correct checksum after defrag" in {
				val disk = Disk.from(Cluster.parseString(sampleInput)).defragCompact
				disk.checksum shouldBe 1928
			}
		}
	}

	"Puzzle2.disk" - {
		import Day9.Puzzle2._
		"for sample input" - {
			"should report correct capacity, total used and total free" in {
				val disk = Disk.from(Cluster.parseString(sampleInput))
				disk.capacity shouldBe 42
				disk.totalUsed shouldBe 28
				disk.totalFree shouldBe 14
			}
			"should report correct checksum before defrag" in {
				val disk = Disk.from(Cluster.parseString(sampleInput))
				disk.checksum shouldBe 4116
			}
			"should report correct checksum after defrag" in {
				val disk = Disk.from(Cluster.parseString(sampleInput)).defragByFile
				disk.checksum shouldBe 2858
			}
		}
	}
}
