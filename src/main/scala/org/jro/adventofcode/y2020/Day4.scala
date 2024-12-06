package org.jro.adventofcode.y2020

import org.jro.adventofcode

import scala.util.Try
import scala.util.matching.Regex

object Day4 extends App {
	sealed trait FieldName {
		def valid(value: String): Boolean
	}
	/*

    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID)

	 */
	object Num {
		def unapply(value: String): Option[Int] = {
			Try(value.toInt).toOption
		}
	}
	object FieldName {
		case object Byr extends FieldName {
			override def valid(value: String): Boolean = isYear(value, 1920, 2002)
		}
		case object Iyr extends FieldName {
			override def valid(value: String): Boolean = isYear(value, 2010, 2020)
		}
		case object Eyr extends FieldName {
			override def valid(value: String): Boolean = isYear(value, 2020, 2030)
		}
		case object Hgt extends FieldName {
			val Size: Regex = """(\d+)(cm|inRules)""".r
			override def valid(value: String): Boolean = value match {
				case Size(Num(s), "cm") => 150 <= s && s <= 193
				case Size(Num(s), "inRules") => 59 <= s && s <=76
				case _ => false
			}
		}
		case object Hcl extends FieldName {
			val HashColor: Regex = """#[0-9a-f]{6}""".r
			override def valid(value: String): Boolean = HashColor.matches(value)
		}
		case object Ecl extends FieldName {
			val AcceptedValues: Set[String] = "amb blu brn gry grn hzl oth".split(" ").toSet
			override def valid(value: String): Boolean = AcceptedValues.contains(value)
		}
		case object Pid extends FieldName {
			val `9digits`: Regex = """\d{9}""".r
			override def valid(value: String): Boolean = `9digits`.matches(value)
		}
		case object Cid extends FieldName {
			@inline
			override def valid(value: String): Boolean = true
		}

		val values: IndexedSeq[FieldName] = IndexedSeq(Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid, Cid)
		val mandatoryFieldNames: Set[FieldName] = Set(values*) - Cid

		def fromSymbol(symbol: String): Option[FieldName] = {
			values.find(_.toString.toLowerCase == symbol.toLowerCase)
		}

		def isYear(value: String, min: Int, max: Int): Boolean = {
			Try(value.toInt).toOption.find(y => min <= y && y <= max).isDefined
		}
	}
	sealed trait Document
	object Document {
		case object Valid extends Document
		case object Invalid extends Document

		def isValid(fieldNames: Set[FieldName]): Boolean = {
			fieldNames.nonEmpty && FieldName.mandatoryFieldNames.forall(fieldNames.contains)
		}

		def ofFields(fieldNames: Set[FieldName]): Document = {
			if(isValid(fieldNames)) Valid else Invalid
		}
	}

	case class Document2(fields: Map[FieldName, String]) {
		def isValid: Boolean = {
			Document.isValid(fields.keySet) && fields.forall {
				case (fn:FieldName, value) => fn.valid(value)
			}
		}
	}


	val DocSeparator: Regex = """\n\n""".r

	def parseInput(txt: String): Seq[Document] = {
		DocSeparator.split(txt).toIndexedSeq.map(parseDocument)
	}

	val FieldSeparator: Regex = """\s""".r
	val FieldValueSeparator: Regex = """:""".r
	def parseDocument(txt: String): Document = {
		Document.ofFields(FieldSeparator.split(txt).flatMap { field =>
			FieldName.fromSymbol(FieldValueSeparator.split(field).head)
		}.toSet)
	}

	def puzzle1(input: String): Int = {
		parseInput(input).count {
			case Document.Valid => true
			case Document.Invalid => false
		}
	}

	def parseInput2(txt: String): Seq[Document2] = {
		DocSeparator.split(txt).toIndexedSeq.map(parseDocument2)
	}

	def parseDocument2(docTxt: String): Document2 = {
		Document2(FieldSeparator.split(docTxt).flatMap { field =>
			val fieldAndValue =  FieldValueSeparator.split(field)
			for {
				fsymbol <- fieldAndValue.headOption
				value <- fieldAndValue.tail.headOption
				fname <- FieldName.fromSymbol(fsymbol)
			} yield (fname, value)
		}.toMap)
	}

	def puzzle2(input: String): Int = {
		parseInput2(input).count(_.isValid)
	}


	val maybeTxt = adventofcode.getInputSourceOf(2020, 4).map { src =>
		src.getLines().mkString("\n")
	}

	val res1 = maybeTxt.map(puzzle1)
	val res2 = maybeTxt.map(puzzle2)

	println(s"res1 = $res1 ; res2 = $res2")

}
