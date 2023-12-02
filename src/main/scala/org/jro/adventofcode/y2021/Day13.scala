package org.jro.adventofcode.y2021

import org.jro.adventofcode
import org.jro.adventofcode.Error.UnexpectedValue
import org.jro.adventofcode.getResourceFolder
import org.jro.adventofcode.y2021.Day13.Fold.{Horizontal, Vertical}

import java.awt.{Color, Graphics2D}
import java.awt.image.BufferedImage
import java.nio.file.Path
import javax.imageio.ImageIO

object Day13 {
	case class Dot(x: Int, y: Int)
	object Dot {
		def from(line: String): Dot = {
			val splitLine = line.split(",")
			Dot(splitLine.head.toInt, splitLine.last.toInt)
		}
	}

	sealed trait Fold {
		def at: Int
		def apply(dot: Dot): Dot
		def partition(dots: Seq[Dot]):(Seq[Dot], Seq[Dot])
	}
	object Fold {
		val HorizontalRegex = """.*y=(\d+)""".r
		val VerticalRegex = """.*x=(\d+)""".r
		case class Horizontal(y: Int) extends Fold {
			override val at = y
			override def apply(dot: Dot): Dot = dot.copy(y = mirror(at, dot.y))
			override def partition(dots: Seq[Dot]): (Seq[Dot], Seq[Dot]) = {
				dots.partition { dot =>
					dot.y < this.y
				}
			}
		}
		case class Vertical(x: Int) extends Fold {
			override val at = x
			override def apply(dot: Dot): Dot = dot.copy(x = mirror(at, dot.x))
			override def partition(dots: Seq[Dot]): (Seq[Dot], Seq[Dot]) = {
				dots.partition { dot =>
					dot.x < this.x
				}
			}
		}

		def mirror(at: Int, value: Int): Int = {
			value + 2 * (at - value)
		}

		def from(line: String): Fold = {
			line match {
				case HorizontalRegex(at) => Horizontal(at.toInt)
				case VerticalRegex(at) => Vertical(at.toInt)
			}
		}
	}


	def parseInputLines(lines: Iterable[String]): (Seq[Dot], Seq[Fold]) = {
		val dots = lines.takeWhile(_.nonEmpty).map(Dot.from).toSeq
		val folds = lines.map(Fold.from).toSeq
		(dots, folds)
	}

	val errOrData: Either[adventofcode.Error, (Seq[Dot], Seq[Fold])] = adventofcode.getInputLines(2021, 13).map(parseInputLines)

	def renderDotsAsTxt(dots: Seq[Dot], fold: Fold): String = {
		val dotMap: Map[(Int, Int), Dot] = dots.map(dot => (dot.x, dot.y) -> dot).toMap
		val xmax = dots.map(_.x).max
		val ymax = dots.map(_.y).max
		(0 to ymax).map { y =>
			(0 to xmax).map { x =>
				dotMap.get((x, y)).map(_ => '#').getOrElse {
					fold match {
						case Horizontal(at) if at == y=> '-'
						case Vertical(at) if at == x => '|'
						case _ => '.'
					}
				}
			} :+ '\n'
		}.flatten.mkString
	}

	def renderAsImage(dots: Seq[Dot], foldNum: Int, fold: Option[Fold] = None, fname: String = "fold"): Either[adventofcode.Error, Path] = {
		val xmax = dots.map(_.x).max
		val ymax = dots.map(_.y).max
		val bimg = new BufferedImage(xmax + 1, ymax + 1, BufferedImage.TYPE_INT_ARGB)
		val g2d: Graphics2D  = bimg.createGraphics()
		g2d.setColor(new Color(0, 0, 0, 0))
		g2d.fillRect(0, 0, bimg.getWidth, bimg.getHeight)
		g2d.setColor(Color.GRAY)
		fold match {
			case Some(Horizontal(at)) => g2d.drawLine(0, at, xmax, at)
			case Some(Vertical(at)) => g2d.drawLine(at, 0, at, ymax)
			case _ => ()
		}
		g2d.setColor(Color.white)
		dots.foreach { dot =>
			g2d.drawLine(dot.x, dot.y, dot.x, dot.y)
		}
		g2d.dispose()
		getResourceFolder(2021, 13).map(path => path.resolve(s"${fname}_$foldNum.png")).flatMap { path =>
			if(ImageIO.write(bimg, "png", path.toFile)) Right(path)
			else Left(UnexpectedValue("ImageIO.write", "true", "false"))
		}
	}

	def puzzle(dots: Seq[Dot], fold: Fold): (Seq[Dot], Int) = {
		println(s"folding=${fold}")
		val (statics, folded) = fold.partition(dots)
		val remaingDots = (folded.map(fold.apply) ++ statics).toSet.toSeq.sortBy[Int] { dot =>
			dot.x * 1000 + dot.y
		}
		(remaingDots, remaingDots.size)
	}

	def puzzle2(dots: Seq[Dot], folds: Seq[Fold]): Seq[Dot] = {
		folds.foldLeft(dots){ (snapshot, fold) =>
			puzzle(snapshot, fold)._1
		}
	}

	def main(args: Array[String]): Unit = {
		val sampleLines = """6,10
						   |0,14
						   |9,10
						   |0,3
						   |10,4
						   |4,11
						   |6,0
						   |6,12
						   |4,1
						   |0,13
						   |10,12
						   |3,4
						   |3,0
						   |8,4
						   |1,10
						   |2,14
						   |8,10
						   |9,0
						   |
						   |fold along y=7
						   |fold along x=5
						   |""".stripMargin.split("\\n")
		val sampleData: (Seq[Dot], Seq[Fold]) = parseInputLines(sampleLines)
		println(sampleData)
		println(renderDotsAsTxt(sampleData._1, sampleData._2.head))
		println(s"Sample: ${puzzle(sampleData._1, sampleData._2.head)}")
		println()

		errOrData.map { data =>
			println(s"Dots=${data._1} (${data._1.size} dots)")
			println(s"Folds=${data._2} (${data._2.size} folds")
			println(s"Snapshot: ${renderAsImage(data._1, 0, Option(data._2.head))}")
			val puzzle1Res = puzzle(data._1, data._2.head)
			println(s"Snapshot: ${renderAsImage(puzzle1Res._1, 1)}")
			println(s"Day13.1: ${puzzle1Res._2}")
			val puzz2Res = puzzle2(data._1, data._2)
			println(s"Day13.2: see image ${renderAsImage(puzz2Res, data._2.length, None, "day13.2")}")
		}
	}
}
