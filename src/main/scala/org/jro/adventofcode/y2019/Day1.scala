package org.jro.adventofcode.y2019

import org.jro.adventofcode.findResourceOpt

import scala.io.{Codec, Source}

object Day1 extends App {
	implicit val codec: Codec = Codec.UTF8

	def requiredFuelForModuleOf(mass: Int): Int = {
		mass / 3 - 2
	}

	def requiredFuelForModuleAndFuel(moduleMass: Int): Int = {
		val requiredFuelForMass = requiredFuelForModuleOf(moduleMass)
		if(requiredFuelForMass <= 0) 0
		else requiredFuelForMass + requiredFuelForModuleAndFuel(requiredFuelForMass)
	}

	def requiredFuelForModules(requiredFuelForUnit: Int => Int)(masses: Iterator[Int]): Int = {
		masses.map(requiredFuelForUnit).sum
	}

	val totalRequiredFuel: Option[Int] = findResourceOpt("2019/day1/input").map {
		Source.fromURL(_)
	}.map { src =>
		src.getLines()
	}.map { lineIter =>
		//requiredFuelForModules(requiredFuelForModuleOf)(lineIter.map(_.toInt)) //Step1
		requiredFuelForModules(requiredFuelForModuleAndFuel)(lineIter.map(_.toInt)) //Step1

	}

	println(s"Total required fuel: $totalRequiredFuel")
}
