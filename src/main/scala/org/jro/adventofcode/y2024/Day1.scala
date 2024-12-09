package org.jro.adventofcode.y2024

import org.jro.adventofcode

object Day1 {

	case class LocationCount(id: Long, count: Long) {
		def isSimilarTo(other: LocationCount): Option[LocationSimilarity] = {
			if (id == other.id) Option(LocationSimilarity(this, other.count))
			else None
		}

	}
	case class LocationSimilarity(location: LocationCount, similarityCount: Long) {
		def merge(other: LocationCount): LocationSimilarity = {
			if (location.id == other.id) copy(similarityCount = similarityCount + 1)
			else this
		}

		def score: Long = location.id * similarityCount * location.count
	}

	private[Day1] def parseLine(line: String): Option[(Long, Long)] = {
		if(line.trim.isEmpty) None
		else {
			val parts = line.split("\\s+")
			Option(parts(0).toLong -> parts(1).toLong)
		}
	}

	private def parseInput(lines: Iterator[String]): (Seq[Long], Seq[Long]) = lines.flatMap(parseLine).toSeq.unzip

	private def sortCols(cols: (Seq[Long], Seq[Long])): (Seq[Long], Seq[Long]) = (cols._1.sorted, cols._2.sorted)

	private def zipCols(cols: (Seq[Long], Seq[Long])): Seq[(Long, Long)] = cols._1.zip(cols._2)

	private def sumDiff(sortedPairs: Seq[(Long, Long)]): Long =
		sortedPairs.foldLeft(0L) { (total, pair) =>
			total + Math.abs(pair._1 - pair._2)
		}

	private def countLocations(locations: Seq[Long]): Map[Long, LocationCount] = {
		locations.foldLeft(Map.empty[Long, LocationCount]) { (counts, locationId) =>
			counts.updatedWith(locationId) {
				case Some(count) => Some(count.copy(count = count.count + 1))
				case None => Some(LocationCount(locationId, 1))
			}
		}
	}

	private def countSimilarities(locations1: Map[Long, LocationCount], locations2: Map[Long, LocationCount]): Map[Long, LocationSimilarity] = {
		locations1.foldLeft(Map.empty[Long, LocationSimilarity]) { (similarities, locationCount) =>
			locations2.get(locationCount._1).map { otherLocation =>
				locationCount._2.isSimilarTo(otherLocation) match
					case Some(similarity) => similarities.updatedWith(similarity.location.id) {
						case Some(existing) => Some(existing.merge(similarity.location))
						case None => Some(similarity)
					}
					case None => similarities
			}.getOrElse(similarities)
		}
	}


	def puzzle1(lines: Iterator[String]): Long = sumDiff(zipCols(sortCols(parseInput(lines))))

	def puzzle2(lines: Iterator[String]): Long = {
		val cols: (Seq[Long], Seq[Long]) = parseInput(lines)
		val locationCounts: (Map[Long, LocationCount], Map[Long, LocationCount]) = (countLocations(cols._1), countLocations(cols._2))
		val locationSimilarities = countSimilarities(locationCounts._1, locationCounts._2)
		locationSimilarities.values.foldLeft(0L) { (total, similarity) =>
			total + similarity.score
		}
	}


	def main(args: Array[String]): Unit = adventofcode.y2024.classicMain(1, puzzle1, puzzle2)
	// OK:	1) 1197984	2) 23387399
}
