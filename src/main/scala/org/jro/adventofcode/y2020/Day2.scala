package org.jro.adventofcode.y2020

import org.jro.adventofcode._
import org.jro.adventofcode.Error.NotMatching
import org.jro.adventofcode.y2020.Day2.PasswdEntry.{isValidForPuzzle1, isValidForPuzzle2, parseIntoPasswdEntry}

import scala.util.matching.Regex

object Day2 extends App {

	case class PasswdEntry(checked: Char, minOccur: Int, maxOccur: Int, passwd: String)
	object PasswdEntry {
		val Regex: Regex = """(\d+)-(\d+) ([a-z]): ([a-z]+)""".r
		def parseIntoPasswdEntry(line: String): Either[Error, PasswdEntry] = {
			line match {
				case PasswdEntry.Regex(min, max, c, pwd) => Right(PasswdEntry(c.charAt(0), min.toInt, max.toInt, pwd))
				case other => Left(NotMatching(other, PasswdEntry.Regex))
			}
		}

		def isValidForPuzzle1(passwdEntry: PasswdEntry): Boolean = {
			val charCount = passwdEntry.passwd.count(_ == passwdEntry.checked)
			passwdEntry.minOccur <= charCount && charCount <= passwdEntry.maxOccur
		}

		def isValidForPuzzle2(passwdEntry: PasswdEntry) : Boolean = {
			(passwdEntry.passwd.charAt(passwdEntry.minOccur - 1) == passwdEntry.checked) !=
			(passwdEntry.passwd.charAt(passwdEntry.maxOccur - 1) == passwdEntry.checked)
		}
	}



	def puzzle(inputLines: Iterator[String], isValid: PasswdEntry => Boolean): Result[Int] = {
		inputLines.map(parseIntoPasswdEntry).foldLeft[Result[Int]](Right(0)) { (maybeCount, maybeEntry) =>
			(maybeCount, maybeEntry) match {
				case (Left(err1:SingleError), Left(err2:SingleError)) => Left(err1 and err2)
				case (left@Left(_), _) => left
				case (Right(count), Right(entry)) => Right(if(isValid(entry)) count + 1 else count)
				case(_, Left(err)) => Left(err)
			}
		}
	}

	/*println(getInputSourceOf(2020, 2).flatMap { src =>
		puzzle(src.getLines(), isValidForPuzzle1)
	})*/
	println(getInputSourceOf(2020, 2).flatMap { src =>
		puzzle(src.getLines(), isValidForPuzzle2)
	})

}
