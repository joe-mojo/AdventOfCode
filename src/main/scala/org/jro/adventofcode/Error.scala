package org.jro.adventofcode

import org.jro.adventofcode.Error.Errors

import scala.reflect.ClassTag
import scala.util.matching.Regex

sealed trait Error {
	infix def and(error: SingleError): Errors
	infix def and(errors: Errors): Errors
}
sealed trait SingleError extends Error {
	infix def and(error: SingleError): Errors = {
		Errors(this, Seq(error))
	}
	infix def and(errors: Errors): Errors = {
		Errors(this, errors.head +: errors.tail)
	}
	def asNEL: Errors = Errors(this)
}
sealed trait ErrorNEL extends Error {
	def head : SingleError
	def tail : Seq[SingleError]
}
object Error {
	sealed trait InputError extends SingleError {
		def input: String
	}
	sealed trait ThrowableError extends SingleError {
		def throwable: Throwable
	}
	case class UnexpectedError(override val throwable: Throwable) extends SingleError with ThrowableError
	case class ResourceNotFound(override val input: String) extends InputError
	case class IOError(override val input: String, override val throwable: Throwable) extends InputError with ThrowableError
	case class NotMatching(override val input: String, regex: Regex) extends InputError
	case class Empty(override val input: String) extends InputError
	case class NaN(override val input: String) extends InputError
	sealed trait WrongSplitError extends InputError {
		def splitRegex: Regex
		def actualParts: Int
	}
	case class WrongFixedSplit(override val input: String, override val splitRegex: Regex, expectedParts: Int, override val actualParts: Int) extends WrongSplitError
	case class WrongVariableSplit(override val input: String, override val splitRegex: Regex, override val actualParts: Int, message: String) extends WrongSplitError
	case class UnknownEnumValue[EnumT: ClassTag](override val input: String) extends InputError {
		val ofType: Class[?] = ClassTag.getClass
	}
	case class UnexpectedValue(label: String, expected: String, actual: String) extends SingleError
	case class Errors(override  val head: SingleError, override val tail: Seq[SingleError] = Seq.empty[SingleError]) extends ErrorNEL {
		def append(error: SingleError): Errors = Errors(this.head, this.tail :+ error)
		def appendAll(errors: Errors): Errors = Errors(this.head, (this.tail :+ errors.head) ++ errors.tail)
		override infix def and(error: SingleError): Errors = {
			Errors(this.head, this.tail :+ error)
		}
		override infix def and(errors: Errors): Errors = {
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
				case (Left(leftErrors:Errors), Left(rightErrors:Errors)) => Left(leftErrors and rightErrors)
			}
		}
	}

	def sequence[T](eithers: IndexedSeq[Either[Error, T]]): Either[Errors, IndexedSeq[T]] = {
		sequence(eithers.iterator).map(_.toIndexedSeq)
	}

}
