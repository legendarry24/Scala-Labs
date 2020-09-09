package spp.olkhovskiy.lab1

import java.net.URLDecoder
import java.time.DayOfWeek
import java.util.{Calendar, TimeZone}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

object Lab1 extends App {
	println(s"Task 1: ${getSignValue(1)}")
	println(s"Task 2: ${calculateCharsProduct("hi")}")
	println(s"Task 3: ${calculateStringCharsProduct("hi")}")
	println(s"Task 4: ${swapAdjacentElements(Array(1, 2, 3, 4, 5)).mkString(" ")}")
	println(s"Task 5: ${swapAdjacentElements2(Array(1, 2, 3, 4, 5)).mkString(" ")}")
	println(s"Task 6: ${orderBySign(Array(1, 4, 0, -3, 0, 5, -1, 2)).mkString(" ")}")
	println(s"Task 7: ${getUniqueElements(Array(1,1,2,2,3,4,5,1,4)).mkString(" ")}")
	println(s"Task 8: ${printTimezones().mkString(", ")}")
	println(s"Task 9: ${reducePriceByTenPercent(Map("PC" -> 5000, "phone" -> 2500)).mkString(" ")}")
	println(s"Task 10: ${getWeekDays(Array("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")).mkString(" ")}")
	println(s"Task 11: ${showWordsFromFile("/words.txt").mkString("\n")}")
	println(s"Task 12: ${displayWordsFromFile("/words.txt").mkString("\n")}")
	println("Task 13: "); displayEnvProperties()
	println(s"Task 14: ${lteqgt(Array(1, 2, 3, 3, 4, 5, 6, 7, 6), 6).toString()}")

	// Task 1
	def getSignValue(number: Int): Int = {
		if (number > 0) 1
		else if (number < 0) -1
		else 0
	}

	// Task 2
	def calculateCharsProduct(inputString: String): Long = {
		if (inputString == null || inputString.isEmpty) 0

		var product: Long = 1
		for (char <- inputString) {
			product *= char
		}

		product
	}

	// Task 3
	def calculateStringCharsProduct(inputString: String): Long = {
		if (inputString == null || inputString.isEmpty) 0

		inputString.product
	}

	//Task 4
	def swapAdjacentElements(array: Array[Int]): Array[Int] = {
		//for (i <- 0 until array.length if i % 2 == 1) {
		for (i <- array.indices if i % 2 == 1) {
			val temp = array(i - 1)
			array(i - 1) = array(i)
			array(i) = temp
		}
		array
	}

	//Task 5
	def swapAdjacentElements2(array: Array[Int]): Array[Int] = {
		(for (i <- array.grouped(2); j <- i.reverse)
			yield j)
			.toArray[Int]
	}

	//Task 6
	def orderBySign(array: Array[Int]): Array[Int] = {
		val positivesArray = ArrayBuffer[Int]()
		val negativesArray = ArrayBuffer[Int]()
		val zerosArray = ArrayBuffer[Int]()

		for (item <- array) {
			if (item > 0) positivesArray += item
			else if (item < 0) negativesArray += item
			else zerosArray += item
		}

		(positivesArray ++ negativesArray ++ zerosArray).toArray
	}

	//Task 7
	def getUniqueElements(array: Array[Int]): Array[Int] = array.distinct

	//Task 8
	def printTimezones(): Array[String] = {
		TimeZone.getAvailableIDs()
			.filter(zone => zone.contains("America"))
    		.map(zone => zone.substring(zone.indexOf('/') + 1))
			.sorted
	}

	//Task 9
	def reducePriceByTenPercent(goods: Map[String, Double]): Map[String, Double] = {
		goods.map(kvp => (kvp._1, kvp._2 * 0.9))
	}

	//Task 10
	def getWeekDays(days: Array[String]): mutable.LinkedHashMap[String, Int] = {
		val map = new mutable.LinkedHashMap[String, Int]()
		for (day <- days) {
			map += (day -> mapToCalendarConstant(day))
		}
		map
	}

	private def mapToCalendarConstant(day: String): Int = {
		DayOfWeek.valueOf(day.toUpperCase()) match {
			case DayOfWeek.SUNDAY => Calendar.SUNDAY
			case DayOfWeek.MONDAY => Calendar.MONDAY
			case DayOfWeek.TUESDAY => Calendar.TUESDAY
			case DayOfWeek.WEDNESDAY => Calendar.WEDNESDAY
			case DayOfWeek.THURSDAY => Calendar.THURSDAY
			case DayOfWeek.FRIDAY => Calendar.FEBRUARY
			case DayOfWeek.SATURDAY => Calendar.SATURDAY
		}
	}

	//Task 11
	def showWordsFromFile(relativeFilePath: String): Map[String, Int] = {
		var path = getClass.getResource(relativeFilePath).getPath
		path = URLDecoder.decode(path, "UTF-8")
		val input = new java.util.Scanner(new java.io.File(path))

		var words = Array[String]()
		while (input.hasNext()) {
			words :+= input.next()
		}

		words.groupBy(identity).view.mapValues(_.length).toMap
	}

	//Task 12
	def displayWordsFromFile(relativeFilePath: String): mutable.Map[String, Int] = {
		var path = getClass.getResource(relativeFilePath).getPath
		path = URLDecoder.decode(path, "UTF-8")
		val input = new java.util.Scanner(new java.io.File(path))

		val words = new java.util.TreeMap[String, Int]()
		var word = ""
		while (input.hasNext()) {
			word = input.next()
			val count: Int = if (words.containsKey(word)) words.get(word) else 0
			words.put(word, count + 1)
		}

		mapToScalaAPI(words)
	}

	private def mapToScalaAPI(javaMap: java.util.Map[String, Int]): mutable.TreeMap[String, Int] = {
		val scalaMap = new mutable.TreeMap[String, Int]()
		javaMap.forEach {
			(k, v) => scalaMap += (k -> v)
		}
		scalaMap
	}

	//Task 13
	def displayEnvProperties(): Unit = {
		val properties: mutable.Map[String, String] = System.getProperties.asScala

		var longestKeyName = ""
		var longestKeyLength = 0

		for ((key, _) <- properties if key.length > longestKeyName.length) {
			longestKeyName = key
			longestKeyLength = key.length
		}
		println(s"The longest key: $longestKeyName. Its length: $longestKeyLength chars.")

		for ((k, v) <- properties) println(s"$k | $v")
	}

	//Task 14
	def lteqgt(values: Array[Int], v: Int): (Int, Int, Int) = {
		var (greaterCount, equalCount, lesserCount) = (0, 0, 0)

		for (value <- values) {
			if (value < v) lesserCount += 1
			else if (value == v) equalCount += 1
			else if (value > v) greaterCount += 1
		}

		(lesserCount, equalCount, greaterCount)
	}
}
