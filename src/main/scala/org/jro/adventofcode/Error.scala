package org.jro.adventofcode

import org.jro.adventofcode.Error.Errors

import scala.reflect.ClassTag
import scala.util.matching.Regex

sealed trait Error {
	def and(error: SingleError): Errors
	def and(errors: Errors): Errors
}
sealed trait SingleError extends Error {
	def and(error: SingleError): Errors = {
		Errors(this, Seq(error))
	}
	def and(errors: Errors): Errors = {
		Errors(this, errors.head +: errors.tail)
	}
}
sealed trait ErrorNEL extends Error
object Error {
	sealed trait InputError extends SingleError {
		def input: String
	}
	sealed trait ThrowableError extends SingleError {
		def throwable: Throwable
	}
	case class ResourceNotFound(override val input: String) extends InputError
	case class IOError(override val input: String, override val throwable: Throwable) extends InputError with ThrowableError
	case class NotMatching(override val input: String, regex: Regex) extends InputError
	case class Empty(override val input: String) extends InputError
	case class NaN(override val input: String) extends InputError
	case class WrongSplit(override val input: String, splitRegex: Regex, expectedParts: Int, actualParts: Int) extends InputError
	case class UnknownEnumValue[EnumT: ClassTag](override val input: String) extends InputError {
		val ofType: Class[_] = ClassTag.getClass
	}
	case class UnexpectedValue(label: String, expected: String, actual: String) extends SingleError
	case class Errors(head: SingleError, tail: Seq[SingleError] = Seq.empty[SingleError]) extends ErrorNEL {
		def append(error: SingleError): Errors = Errors(this.head, this.tail :+ error)
		def appendAll(errors: Errors): Errors = Errors(this.head, (this.tail :+ errors.head) ++ errors.tail)
		override def and(error: SingleError): Errors = {
			Errors(this.head, this.tail :+ error)
		}
		override def and(errors: Errors): Errors = {
			Errors(this.head, (this.tail :+ errors.head) ++ errors.tail)
		}
	}

	def sequence[T](eithers: Iterator[Either[Error, T]]): Either[Errors, Seq[T]] = {
		eithers.foldLeft[Either[Errors, Seq[T]]](Right(Seq.empty[T])){ (memo, elt) =>
			(memo, elt) match {
				case (Right(seq), Right(t)) => Right(seq :+ t)
				case (Right(_), Left(err:Errors)) => Left(err)
				case (Right(_), Left(err:SingleError)) => Left(Errors(err))
				case (l@Left(_), Right(_)) => l
				case (Left(errors:Errors), Left(error:SingleError)) => Left(errors and error)
			}
		}
	}

	def sequence[T](eithers: IndexedSeq[Either[Error, T]]): Either[Errors, IndexedSeq[T]] = {
		sequence(eithers.iterator).map(_.toIndexedSeq)
	}

}