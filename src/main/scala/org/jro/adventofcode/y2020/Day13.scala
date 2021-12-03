package org.jro.adventofcode.y2020

object Day13 {



	def main(args: Array[String]): Unit = {
		val earliestTimestamp = 1008713
		val busIds = "13,x,x,41,x,x,x,x,x,x,x,x,x,467,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,29,x,353,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,23".
			split(""",""").filter(_ != "x").map(_.toInt)

		var resTimestamp = earliestTimestamp;
		var compatibleBuses = busIds.filter(busId => (resTimestamp % busId) == 0)
		while(compatibleBuses.isEmpty) {
			resTimestamp +=1
			compatibleBuses = busIds.filter(busId => (resTimestamp % busId) == 0)
		}
		val res1 = (resTimestamp - earliestTimestamp) * compatibleBuses.head

		println(s"found: ${compatibleBuses.toSeq} @ $resTimestamp => res1=$res1")

	}

}
