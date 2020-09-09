package spp.olkhovskiy.lab5

import java.util.Arrays._
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import java.awt.Color

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props

import scala.collection.mutable
import scala.io.Source
import scala.util.{Random, Using}
import scala.util.matching.Regex

object Main extends App {
	// Task 1

	// Single thread
	val n: Int = 1000000
	var testArray: Array[Int] = generateRandomIntArray(n)
	var ts: Long = System.currentTimeMillis()
	println(testArray.sum / testArray.length)
	println(s"Elapsed time: ${System.currentTimeMillis() - ts} millis.")

	// MultiThread
	val system = ActorSystem("lab5")
	val combiner = system.actorOf(Props(classOf[Combiner], 3))
	val calculator1 = system.actorOf(Props(classOf[Calculator], combiner))
	val calculator2 = system.actorOf(Props(classOf[Calculator], combiner))
	val calculator3 = system.actorOf(Props(classOf[Calculator], combiner))

	testArray = generateRandomIntArray(n)
	ts = System.currentTimeMillis()

	calculator1 ! (copyOfRange(testArray, 0, 333333), ts)
	calculator2 ! (copyOfRange(testArray, 333333, 666666), ts)
	calculator3 ! (copyOfRange(testArray, 666666, n), ts)

	// Task 2 + 3
	val originalFile = "src/main/resources/img/Sunflower.jpg"
	val invertedFile = "src/main/resources/img/inv_Sunflower.jpg"
	val img: BufferedImage = ImageIO.read(new File(originalFile))

	//val system = ActorSystem("task2")
	val imageRowCombiner = system.actorOf(Props(classOf[ImageRowCombiner], img, new File(invertedFile)))
	val evenRowInverter = system.actorOf(Props(classOf[RowColorInverter], imageRowCombiner))
	val oddRowInverter = system.actorOf(Props(classOf[RowColorInverter], imageRowCombiner))

	for (row <- 0 until img.getHeight)
		if (row % 2 == 0)
			evenRowInverter ! (row, createPixelsArray(row, img))
		else
			oddRowInverter ! (row, createPixelsArray(row, img))

	def createPixelsArray(row: Int, img: BufferedImage): Array[Int] = {
		(for (i <- 0 until img.getWidth) yield img.getRGB(i, row)).toArray.reverse
	}

	// Tasks 4-6
	//val system = ActorSystem("task4")
	val master = system.actorOf(Props(new WorkManager))
	master ! new File("src/main/resources/tmp")


	// Task 1
	class Combiner(val calculatorsCount: Int) extends Actor {
		private var cache: List[Int] = List.empty

		override def receive: Receive = {
			case (partialAvg: Int, ts: Long) =>
				cache = partialAvg :: cache
				if (cache.length >= calculatorsCount) {
					println(cache.sum / calculatorsCount)
					println(s"Elapsed time: ${System.currentTimeMillis() - ts} millis.")
				}
		}
	}

	class Calculator(val combiner: ActorRef) extends Actor {
		override def receive: Receive = {
			case (data: Array[Int], ts: Long) => combiner ! (data.sum / data.length, ts)
		}
	}

	def generateRandomIntArray(size: Int): Array[Int] = Seq.fill(size)(Random.nextInt).toArray

	// Task 2 + 3
	class ImageRowCombiner(val img: BufferedImage, val invertFile: File) extends Actor {
		private val countRows = img.getHeight
		private var countReplacedRows = 0

		override def receive: Receive = {
			case (row: Int, colors: Array[Color]) =>
				replaceRowColor(row, colors)
				countReplacedRows += 1
				if (countRows == countReplacedRows)
					ImageIO.write(img, "jpg", invertFile)
		}

		def replaceRowColor(row: Int, colors: Array[Color]): Unit =
			if (colors.length == 1)
				img.setRGB(colors.length - 1, row, colors(0).getRGB)
			else {
				img.setRGB(colors.length - 1, row, colors(0).getRGB)
				replaceRowColor(row, colors.drop(1))
			}
	}

	class RowColorInverter(val combiner: ActorRef) extends Actor {
		private val white = 255

		override def receive: Receive = {
			case (row: Int, pixels: Array[Int]) =>
				val colors = pixels.map(new Color(_, true))
					.map(color => new Color(
						white - color.getRed,
						white - color.getGreen,
						white - color.getBlue
					))
				combiner ! (row, colors)
		}
	}

	// Tasks 4-6
	case class RegexMatchResult(value: String, filePath: String)

	class ResultsCombiner(manager: ActorRef) extends Actor with Worker {
		private var filesCount = 0
		private var regexMatches: List[RegexMatchResult] = List()

		override val master: ActorRef = manager

		override def receive: Receive = {
			case matches: List[RegexMatchResult] =>
				regexMatches = regexMatches ++ matches
				filesCount += 1
			case "result" =>
				println(s"Files: $filesCount")
				println(s"Matches: ${regexMatches.size}")
				regexMatches = regexMatches.sortWith(_.value < _.value)
				var lastName = ""
				var lastPlace = ""
				for(r <- regexMatches) {
					if (lastName != r.value) {
						println(s"Found ${r.value} in ${r.filePath}")
						lastName = r.value
						lastPlace = r.filePath
					} else if (lastPlace != r.filePath) {
						println(s"\tAnd ${r.filePath}")
					}
				}
				master ! "finish"
		}
	}

	class FileScanner(manager: ActorRef, combiner: ActorRef, regexPattern: Regex) extends Actor with Worker {
		override val master: ActorRef = manager

		override def receive: Receive = {
			case file: File =>
				var regexMatches: List[RegexMatchResult] = List()
				Using(Source.fromFile(file, "UTF-8")) {
					source => {
						val fileContent: String = source.getLines.mkString
						for (m: Regex.Match <- regexPattern.findAllMatchIn(fileContent)){
							regexMatches = regexMatches :+ RegexMatchResult(m.toString, file.getPath)
						}
					}
				}

				combiner ! regexMatches
				master ! ("scannerSuccess", self)
		}
	}

	class FileSearcher(manager: ActorRef) extends Actor with Worker {
		override val master: ActorRef = manager
		private var files: List[File] = List.empty[File]

		override def receive: Receive = {
			case _: File =>
				files = getFilesRecursive(new File("src/main/resources/txt/")).filter(_.isFile).toList
				println(s"Found files: ${files.mkString(", ")}")
				master ! ("getScanners", files.length)
			case Seq(scanners @_*) if scanners.nonEmpty && scanners.head.isInstanceOf[ActorRef] =>
				for (i <- files.indices)
					scanners(i).asInstanceOf[ActorRef] ! files(i)
		}

		def getFilesRecursive(f: File): Array[File] = {
			val files: Array[File] = f.listFiles
			files ++ files.filter(_.isDirectory).flatMap(getFilesRecursive)
		}
	}

	trait Worker {
		val master: ActorRef
	}

	class WorkManager extends Actor {
		private val scannerStatuses = mutable.Map.empty[ActorRef, String]
		private val digitPattern = """[0-9]+""".r
		val searcher: ActorRef = context.system.actorOf(Props(new FileSearcher(self)), "searcher")

		override def receive: Receive = {
			case dir: File => context.system.actorSelection("/user/searcher") ! dir
			case ("getScanners", count: Int) =>
				val combiner = context.system.actorOf(Props(new ResultsCombiner(self)), "combiner")
				val scanners: Seq[ActorRef] = for (i <- 0 until count) yield {
					context.system.actorOf(Props(new FileScanner(self, combiner, digitPattern)), s"scanner$i")
				}
				scanners.foreach(scanner => scannerStatuses += (scanner -> "running"))
				context.system.actorSelection("/user/searcher") ! scanners
			case ("scannerSuccess", scanner: ActorRef) =>
				context.system.stop(scanner)
				scannerStatuses(scanner) = "ok"
				if (scannerStatuses.count(s => s._2 == "running") == 0) {
					context.system.actorSelection("/user/combiner") ! "result"
				}
			case "finish" => context.system.terminate()
		}
	}
}
