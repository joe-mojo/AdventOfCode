package org.jro.adventofcode.y2021

import org.jro
import org.jro.adventofcode
import org.jro.adventofcode.Error
import org.jro.adventofcode.getInputLines
import org.jro.adventofcode.y2021.Cave.Start

import scala.annotation.targetName
import scala.collection.mutable.Map as MutMap

sealed abstract class Cave{
	def name: String
}

object Cave {
	sealed trait S extends Cave
	sealed trait L extends Cave
	case object Start extends S {
		override val name = "start"
	}
	case object End extends S {
		override val name = "end"
	}
	case class Small private[Cave](override val name: String) extends S
	case class Large private[Cave](override val name: String) extends L

	def createCaveWithName(name: String): Cave = {
		name match
			case "start" => Start
			case "end" => End
			case other if other.toUpperCase() == other => Large(other)
			case other => Small(other)
	}

}

/*case class Path(caves: Seq[Cave]){
	@targetName("append")
	def `+`(cave: Cave): Path = Path(caves :+ cave)
	def
}*/

case class Arc(cave1: Cave, cave2: Cave) {
	override def equals(obj: Any): Boolean = {
		Option(obj) match {
			case Some(Arc(c1, c2)) => (cave1 == c1 && cave2 == c2) || (cave2 == c1 && cave1 == c2)
			case _ => false
		}
	}

	override def hashCode(): Int = cave1.hashCode() | cave2.hashCode()

}

object Arc {
	def apply(cave1: Cave, cave2: Cave): Arc = new Arc(cave1, cave2)

	def apply(caves: Seq[Cave]): Arc = new Arc(caves.head, caves.last)
}


type Path = Seq[Cave]
/**
 * @author joe_mojo.
 *         2023/11/30
 */
object Day12 {

	def parseLine(line: String): Arc =
		Arc(line.split("-").map(Cave.createCaveWithName))

	def loadInput(): Either[Error, Map[Cave, Set[Cave]]] = {
		getInputLines(2021, 12).map(_.map(parseLine).foldLeft(Map.empty[Cave, Set[Cave]]){ (map, arc) =>
			map.updatedWith(arc.cave1){
				case Some(caves) => Option(caves + arc.cave2)
				case None => Option(Set(arc.cave2))
			}.updatedWith(arc.cave2){
				case Some(caves) => Option(caves + arc.cave1)
				case None => Option(Set(arc.cave1))
			}
		})
	}

	def exploreFrom(cave: Cave, caves:Map[Cave, Set[Cave]], seen: Set[Cave], currentPath: Seq[Cave], paths: Set[Path]): Set[Path] = {
		val newSeen = seen + cave
		//caves(cave).filter(c => )
		???
	}


	def explore(caves: Map[Cave, Set[Cave]]): Set[Path] = {
		exploreFrom(Start, caves, Set.empty[Cave], Seq(Start), Set.empty[Path])
	}

	//val errOrData: Either[adventofcode.Error, /*TODO*/] = adventofcode.getInputLines(2021, 13).map(parseInputLines)

	def main(args: Array[String]): Unit = {

	}

}
