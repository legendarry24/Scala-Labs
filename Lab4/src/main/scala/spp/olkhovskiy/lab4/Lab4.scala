package spp.olkhovskiy.lab4

import java.io.{BufferedInputStream, FileInputStream, InputStream}
import java.net.URLDecoder

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Lab4 {
	def main(args: Array[String]): Unit = {
		// Task 1
		println("Task 1:")

		var path = getClass.getResource("/words.txt").getPath
		path = URLDecoder.decode(path, "UTF-8")
		val inputStream = new FileInputStream(path) with BufferedStream
		var byteArray = new Array[Byte](1024)
		val readBytesNumber = inputStream.read(byteArray)
		byteArray = byteArray.slice(0, readBytesNumber)

		println("File content: ")
		byteArray.foreach(x => print(x.toChar))

		// Task 2
		println("\nTask 2:")

		println(s"15/-6 = ${Fraction(15, -6)}")
		println(s"45/15 = ${Fraction(45, 15)}")

		// Task 3
		println("Task 3:")
		println(s"Is 1.75 + 0.50 equal to 2.25: " +
				s"${new Money(1, 75) + new Money(0, 50) == new Money(2, 25)}")

		// Task 4
		println("\nTask 4:")
		println(Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET")

		// Task 5
		println("\nTask 5:")
		val matrix1 = new Matrix(
			Array(
				Array(1, 2, 3),
				Array(4, 5, 6),
				Array(7, 8, 9)
			)
		)
		println(s"First matrix: \n$matrix1\n")

		val matrix2 = new Matrix(
			Array(
				Array(2, 2, 3),
				Array(4, 4, 5),
				Array(6, 6, 8)
			)
		)
		println(s"Second matrix: \n$matrix2\n")

		println(s"Matrix multiplication: \n${matrix1 * matrix2}\n")
		println(s"Scalar product by 2: \n${matrix1 * 2}\n")
		println(s"Matrix (0,0) element: ${matrix1(0, 0)}")

		// Task 6
		println("Task 6:")

		val (func, min, max) = ((x: Int) => x * x, -5, 5)
		println(s"Apply func x => x * x with range from $min to $max: ${values(func, -5, 5)}")

		// Task 7
		println("Task 7:")

		println(s"Max value of func x => 10 * x - x * x on range from 1 to 10: " +
				s"${largest(x => 10 * x - x * x, 1 to 10)}")

		// Task 8
		println("Task 8:")

		val word = "Mississippi"
		println(s"Indices of the word '$word': ${indices(word)}")

		// Task 9
		println("Task 9:")

		val list = ListBuffer(0, 1, 2, 3, 0, 0, 4, 0, 5, 6, 0, 0, 7)
		println(s"Initial list: ${list.mkString(", ")}.\n" +
				s"List after removing all zeroes: ${removeAllZeroes(list).mkString(", ")}")
	}

	// Task 1
	trait BufferedStream {
		this: InputStream =>
		private val streamDecorator = new BufferedInputStream(this, 2048)

		override def read(byteArray: Array[Byte]): Int = {
			streamDecorator.read(byteArray)
		}
	}

	// Task 2
	case class Fraction(_numerator: Int, _denominator: Int) {
		private val _sign: Int = _numerator.sign * _denominator.sign
		private val _gcd: Int = getGCD(_numerator.abs, _denominator.abs)
		val numerator: Int = _numerator.abs * _sign / _gcd
		val denominator: Int = _denominator.abs / _gcd

		def +(that: Fraction): Fraction = {
			Fraction(numerator * that.denominator + that.numerator * denominator, denominator * that.denominator)
		}

		def -(that: Fraction): Fraction = {
			Fraction(numerator * that.denominator - that.numerator * denominator, denominator * that.denominator)
		}

		def *(that: Fraction): Fraction = {
			Fraction(numerator * that.numerator, denominator * that.denominator)
		}

		def /(that: Fraction): Fraction = {
			Fraction(numerator * that.denominator, denominator * that.numerator)
		}

		@scala.annotation.tailrec
		private def getGCD(a: Int, b: Int): Int = {
			if (b == 0) a else getGCD(b, a % b)
		}

		override def toString: String = numerator + "/" + denominator
	}

	// Task 3
	class Money(val dollars: Int, val cents: Int) {
		final def +(that: Money): Money = {
			if (this.cents + that.cents > 100) {
				new Money(this.dollars + that.dollars + 1, this.cents + that.cents - 100)
			} else {
				new Money(this.dollars + that.dollars, this.cents + that.cents)
			}
		}

		final def -(that: Money): Money = new Money(this.dollars - that.dollars, this.cents - that.cents)

		final def ==(that: Money): Boolean = this.dollars == that.dollars && this.cents == that.cents

		final def <(that: Money): Boolean = this.dollars < that.dollars && this.cents < that.cents
	}

	// Task 4
	class Table {
		private val items = ArrayBuffer[ArrayBuffer[String]](ArrayBuffer())

		// append table data
		def |(s: String): Table = {
			items(items.length - 1).append(s)
			this
		}

		// append table row
		def ||(s: String): Table = {
			items += ArrayBuffer(s)
			this
		}

		override def toString: String = {
			items.map(_.mkString("<td>", "</td><td>", "</td>")).mkString("<table>\n\t<tr>", "</tr>\n\t<tr>", "</tr>\n</table>")
		}
	}

	object Table {
		def apply() = new Table
	}

	// Task 5
	class Matrix(val matrix: Array[Array[Double]]) {
		private val rowsCount: Int = matrix.length
		private val columnsCount: Int = matrix(0).length

		def apply(row: Int, col: Int): Double = matrix(row)(col)

		def +(that: Matrix): Matrix = new Matrix(compute(that, (a, b) => a + b))

		def *(that: Matrix): Matrix = new Matrix(compute(that, (a, b) => a * b))

		def *(value: Double): Matrix = {
			new Matrix(
				(for (i <- matrix.indices)
					yield matrix(i).map(_ * value))
					.toArray
			)
		}

		def dim: (Int, Int) = (rowsCount, columnsCount)

		override def toString: String = matrix.map(_.mkString("( ", " ", " )")).mkString("\n")

		private def compute(that: Matrix, operation: (Double, Double) => Double): Array[Array[Double]] = {
			if (that.dim != dim) throw new Exception("Matrices must be of the same size")
			(for (i <- matrix.indices)
				yield matrix(i).zip(that.matrix(i)).map(pair => operation(pair._1, pair._2))
				).toArray
		}
	}

	// Task 6
	def values(func: Int => Int, min: Int, max: Int): Seq[(Int, Int)] = {
		for (i <- min to max) yield (i, func(i))
	}

	// Task 7
	def largest(func: Int => Int, inputs: Seq[Int]): Int = inputs.map(func).max

	// Task 8
	def indices(inputString: String): mutable.Map[Char, mutable.Set[Int]] = {
		val map = mutable.SortedMap[Char, mutable.Set[Int]]()

		for ((char, index) <- inputString.zipWithIndex)
			map.getOrElseUpdate(char, mutable.SortedSet[Int]()) += index

		map
	}

	// Task 9
	def removeAllZeroes(list: mutable.ListBuffer[Int]): mutable.ListBuffer[Int] = {
		list.filter(_ != 0)
	}
}
