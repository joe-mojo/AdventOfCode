package org.jro

import java.net.URL
import org.jro.adventofcode.Error.{IOError, NaN, ResourceNotFound, ThrowableError}

import java.io.{BufferedWriter, File, FileWriter, StringWriter}
import java.nio.file.Path
import java.time.Year
import scala.io.Source
import scala.util.{Try, Using}

package object adventofcode {
	type Result[T] = Either[Error, T]
	def findResourceOpt(resource: String): Option[URL] = {
		Option(Thread.currentThread().getContextClassLoader.getResource(resource))
	}

	def findResource(resource: String): Either[Error, URL] = {
		Option(Thread.currentThread().getContextClassLoader.getResource(resource))
			.fold[Result[URL]](Left(ResourceNotFound(resource)))(Right(_))
	}

	def getSource(resource: String): Either[Error, Source] = {
		findResource(resource).map(Source.fromURL(_))
	}

	def getResourceNameOf(year: Int, day: Int): String = {
		s"${year}/day${day}/input"
	}

	def getInputSourceOf(year: Int, day: Int): Either[Error, Source] = {
		getSource(getResourceNameOf(year, day))
	}

	case class InputData(year: Int, day: Int, resource: String, source: Source) extends AutoCloseable {
		override def close(): Unit = source.close()
	}
	def getInputData(year: Int, day: Int): Either[Error, InputData] = {
		val resource = getResourceNameOf(year, day)
		getSource(resource).map { source =>
			InputData(year, day, resource, source)
		}
	}

	def getInputLines(year: Int, day: Int): Either[Error, Iterable[String]] = {
		getInputData(year, day).map(_.source.getLines().to(Iterable))
	}
	
	def getInputLinesOrThrow(year: Int, day: Int): Iterable[String] = {
		getInputLines(year, day) match
			case Right(lines) => lines
			case Left(err) => throw new RuntimeException(err.toString)
	}

	def parseInt(value: String): Either[NaN, Int] = {
		Try(value.toInt).fold[Either[NaN, Int]](_ => Left(NaN(value)), Right(_))
	}

	def parseLong(value: String): Either[NaN, Long] = {
		Try(value.toLong).fold[Either[NaN, Long]](_ => Left(NaN(value)), Right(_))
	}

	def writeToFile(lines: IterableOnce[String], outputPath: Path): Either[Error, Unit] = {
			Try {
				val w = new BufferedWriter(new FileWriter(outputPath.toFile), 8*1024)
				lines.iterator.foreach(w.write)
				w.close()
			}.toEither.left.map(err => IOError(outputPath.toString, err))
	}

	def getResourceFolder(year: Int, day: Int): Either[Error, Path] = {
		findResource(s"$year/day$day").map(url => Path.of(url.toURI))
	}
	
	def classicMain[R](year: Int)(day: Int, puzzle1: Iterator[String] => R = (iter) => ???, puzzle2: Iterator[String] => R = (iter) => ???): Unit = {
		getInputLines(year, day) match
			case Right(lines) =>
				println(s"Puzzle 1 = ${puzzle1(lines.iterator)}")
				println(s"Puzzle 2 = ${puzzle2(lines.iterator)}")
			case Left(err) =>
				println(s"Puzzle input didn't load ! Reason:\n $err")
	}

	def defaultInputTransformer(inputData: InputData): Either[Error, IndexedSeq[String]] = {
		Try(inputData.source.getLines().toIndexedSeq).
				toEither.left.map(throwable => IOError(inputData.resource, throwable))
	}

	def mainWithTransformer[R, I](year: Int)(
			day: Int,
			puzzle1: I => R = (iter: R) => ???,
			puzzle2: I => R = (iter: R) => ???,
			inputTransformer: InputData => Either[Error, I] = defaultInputTransformer
	): Unit = {
		getInputData(year, day).flatMap { inputData =>
			Using(inputData) { inputData =>
				inputTransformer(inputData).map { formatedInput =>
					println(s"Puzzle 1 = ${puzzle1(formatedInput)}")
					println(s"Puzzle 2 = ${puzzle2(formatedInput)}")
				}
			}.fold(throwable => Left(IOError(inputData.resource, throwable)), identity)
		} match
			case Right(_) =>
				println("Done")
			case Left(err:ThrowableError) =>
				println(s"Error: ${err}\nStack trace:\n${err.throwable.getStackTrace.toList.mkString("\t", "\n\t", "")}")
			case Left(err) =>
				println(s"Error: $err")
	}
}
