package org.jro.adventofcode.y2023

object Day6 {
	private val InputData = """Time:        40     82     84     92
		  					   |Distance:   233   1011   1110   1487
		  					   |""".stripMargin

	private val SampleData = """Time:      7  15   30
							   |Distance:  9  40  200
							   |""".stripMargin

	case class RaceParam(pushTime: Int, travelTime: Int) {
		def race: Race = Race(pushTime + travelTime, pushTime * travelTime)
	}

	case class Race(duration: Int, distance: Int)

	case class RaceRecord(time: Int, distance: Int) {

		def generateRaceParams: Iterator[RaceParam] = {
			for {
				pushTime <- (1 until time).iterator
			} yield RaceParam(pushTime, time - pushTime)
		}

		def findBetterRaces: Iterator[Race] = {
			generateRaceParams.map(_.race).withFilter(_.distance > this.distance)
		}
	}

	sealed trait Input {
		def raceRecords: Seq[RaceRecord]
	}

	object Input {
		object Main extends Input {
			override val raceRecords: Seq[RaceRecord] = Seq(RaceRecord(40, 233), RaceRecord(82, 1011), RaceRecord(84, 1110), RaceRecord(92, 1497))
		}

		object Sample extends Input {
			override val raceRecords: Seq[RaceRecord] = Seq(RaceRecord(7, 9), RaceRecord(15, 40), RaceRecord(30, 200))
		}
	}

	def puzzle1(input: Input): Long = {
		input.raceRecords.map(_.findBetterRaces.length.toLong).product
	}


	def main(args: Array[String]): Unit = {
		println(s"Sample: ways to beat records = ${puzzle1(Input.Sample)}") // 288, OK
		println(s"Actual input: ways to beat records = ${Input.Main.raceRecords.map(rec => rec.generateRaceParams.map(_.race).withFilter(_.distance > rec.distance).length)}")
		println(s"Actual input: puzzle1 result = ${puzzle1(Input.Main)}") //FIXME 3186225 is too low
	}
}
