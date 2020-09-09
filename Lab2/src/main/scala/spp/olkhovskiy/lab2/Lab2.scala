package spp.olkhovskiy.lab2

import java.lang.System._
import java.time.LocalDateTime._
import java.time.Month
import java.util
import java.util.{HashMap => JavaHash}

import scala.Console.{err, in}
import scala.beans.BeanProperty
import scala.collection.immutable.{HashMap => ScalaHash}
import scala.util.control.Breaks._
import scala.util.matching.Regex

object Lab2 {
	def main(args: Array[String]): Unit = {
		println("Task 1:")

		val time1 = new Time(3, 22)
		val time2 = new Time(2, 15)
		println(time1.before(time2))

		val time3 = new Time2(1, 10)
		val time4 = new Time2(23, 55)
		println(time3.before(time4))

		//val time = new Time(-3, 22)

		println("Task 2:")
		val person = new Person("Fred Smith")
		println(s"first name: ${person.firstName}, last name: ${person.lastName}")

		println("Task 3:")
		val car1 = Car("manufacturer", "model")
		val car2 = Car("manufacturer", "model", 2000)
		val car3 = Car("manufacturer", "model", 2005, "XA0101")
		val car4 = Car("manufacturer", "model", number = "XA0202")
		val cars = Seq(car1, car2, car3, car4)
		cars.foreach{
			car => println(s"manufacturer: ${car.manufacturer}, model: ${car.model}, year: ${car.year}, number: ${car.number}")
		}

		println("Task 4:")
		val car5 = Car("BMW", "735")
		println(s"manufacturer: ${car5.manufacturer}, model: ${car5.model}, year: ${car5.year}, number: ${car5.number}")

		println("Task 5:")
		println(RGBCube.BLUE)

		println("Task 6:")
		val javaMap = new util.HashMap[Int, String]()
		javaMap.put(1, "one")
		javaMap.put(2, "two")
		javaMap.put(3, "three")
		println(CollectionUtils.toScalaHashMap(javaMap).mkString(", "))

		println("Task 7:")
		authenticate() // example: CorrectPass1@

		println("Task 8:")
		val checkingAccount = new CheckingAccount(100)
		checkingAccount.deposit(50)  // 149
		checkingAccount.withdraw(75) // 73
		checkingAccount.deposit(25)  // 97
		println(checkingAccount.balance) //must be 97

		println("Task 9:")
		val savingsAccount = new SavingsAccount(100)
		savingsAccount.deposit(10) // free 110
		savingsAccount.withdraw(20) // free 90
		savingsAccount.withdraw(15) // free 75
		savingsAccount.withdraw(10) // paid 64
		savingsAccount.withdraw(10) // paid 53
		println(savingsAccount.balance) // must be 53

		println("Task 10:")
		val rectangle = new Rectangle((10, 10), 30, 40)
		val circle = new Circle((10, 10), 10)

		println(rectangle.centerPoint)
		println(circle.centerPoint)
	}

	// Task 7
	def authenticate(): Unit = {
		val strongPasswordPattern: Regex = "^(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z])(?=.*[@#$%^&+=])(?=\\S+$).{8,}$".r
		val username: String = getProperty("user.name")

		breakable {
			while (true) {
				out.print("Please, enter a password: ")
				val password: String = in.readLine()

				strongPasswordPattern.findFirstMatchIn(password) match {
					case Some(_) =>
						println(s"Welcome, $username!")
						break()
					case None => err.println("Weak password. Password must contain at least: a digit, a lowercase letter, " +
						"an uppercase letter, a special character and at least 8 characters long.")
				}
			}
		}
	}
}

// Task 1
class Time(@BeanProperty val hours: Int, val minutes: Int) {
	if (hours < 0) {
		throw new IllegalArgumentException
	}

	def before(other: Time): Boolean = {
		(this.hours < other.hours) || (this.hours == other.hours && this.minutes < other.minutes)
	}
}

class Time2(hours: Int, minutes: Int) {
	private val minutesOfTheDay: Int = hours * 60 + minutes

	def before(other: Time2): Boolean = {
		this.minutesOfTheDay < other.minutesOfTheDay
	}
}

// Task 2
class Person(fullName: String) {
	val firstName: String = fullName.split(" ").head
	val lastName: String = fullName.split(" ").last
}

// Task 3
class Car(val manufacturer: String,
		  val model: String,
		  val year: Int = -1,
		  var number: String = "") {
}

// Task 4
object Car {
	def apply(manufacturer: String, model: String, year: Int = -1, number: String = ""): Car = {
		new Car(manufacturer, model, year, number)
	}
}

// Task 5
object RGBCube {
	sealed abstract class Color(val name: String, val code: Int) {
		override def toString: String = f"Name: $name. Code: $code%X"
	}

	case object BLACK extends Color("Black", 0x000000)

	case object BLUE extends Color("Blue", 0x0000FF)

	case object RED extends Color("Red", 0xFF0000)

	case object MAGENTA extends Color("Magenta", 0xFF00FF)

	case object GREEN extends Color("Green", 0x00FF00)

	case object CYAN extends Color("Cyan", 0x00FFFF)

	case object YELLOW extends Color("Yellow", 0xFFFF00)

	case object WHITE extends Color("White", 0xFFFFFF)
}

// Task 6
object CollectionUtils {
	def toScalaHashMap[K, V](javaMap: JavaHash[K, V]): Map[K, V] = {
		var scalaMap = new ScalaHash[K, V]()
		javaMap.forEach((k, v) => {
			scalaMap += (k -> v)
		})
		scalaMap
	}
}

// Task 8
class BankAccount(initialBalance: Double) {
	private var _balance = initialBalance

	def deposit(amount: Double): Double = {
		_balance += amount; _balance
	}
	def withdraw(amount: Double): Double = {
		_balance -= amount; _balance
	}

	def balance: Double = _balance
}

class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
	def this() {
		this(0)
	}

	override def deposit(amount: Double): Double = {
		super.deposit(amount - 1)
	}

	override def withdraw(amount: Double): Double = {
		super.withdraw(amount + 1)
	}
}

// Task 9
object DateUtils {
	def getCurrentMonth: Month = now().getMonth

	def isMonthInPast(month: Month): Boolean = {
		getCurrentMonth.compareTo(month) < 0
	}
}

class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance) {
	private val NUMBER_OF_FREE_OPERATIONS = 3

	private var currentMonth = DateUtils.getCurrentMonth
	private var freeOperationsCounter = NUMBER_OF_FREE_OPERATIONS

	override def deposit(amount: Double): Double = {
		super.deposit(earnMonthlyInterest(amount, -1))
	}

	override def withdraw(amount: Double): Double = {
		super.withdraw(earnMonthlyInterest(amount, 1))
	}

	private def earnMonthlyInterest(amount: Double, fee: Double): Double = {
		checkDate()
		if (freeOperationsCounter == 0) {
			amount + fee
		} else {
			freeOperationsCounter -= 1
			amount
		}
	}

	private def checkDate(): Unit = {
		if (DateUtils.isMonthInPast(currentMonth)) {
			updateFreeOperations()
		}
	}

	private def updateFreeOperations(): Unit = {
		currentMonth = DateUtils.getCurrentMonth
		freeOperationsCounter = NUMBER_OF_FREE_OPERATIONS
	}
}

// Task 10
abstract class Shape {
	def centerPoint: (Int, Int)
}

class Rectangle(upperLeft: (Int, Int), width: Int, height: Int) extends Shape {
	override def centerPoint: (Int, Int) = {
		(upperLeft._1 + width / 2, upperLeft._2 + height / 2)
	}
}

class Circle(val centerPoint: (Int, Int), rad: Int) extends Shape {
	//override def centerPoint: (Int, Int) = this.center
}