package alokhina.lab1

import alokhina.lab1.task11.{getClass, w_count}
import alokhina.lab1.task7.result
import alokhina.lab1.task8.result

import java.util.TimeZone
import java.util.Calendar
import scala.io.StdIn
import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.collection.mutable.{Map => MutableMap}
import scala.collection.mutable.Map

object task1 extends App{
  // Знак числа дорівнює 1, якщо число позитивне,
  // -1 - якщо негативне, і 0 - якщо воно дорівнює нулю.
  // Напишіть функцію, яка обчислює це значення.

  println("Hi! Enter your number:")
  val x = StdIn.readFloat()
  val result = if (x > 0) { 1
  } else if (x == 0) 0 else -1
  println(result)
}

object task2 extends App{
  // Напишіть цикл for для обчислення добутку кодових
  // пунктів Юникода всіх букв в рядку.

  val string = StdIn.readLine("Hi! Enter your string:")
  var result = 1
  for (i <- string) result *= i.toInt

  println(result)
}

object task3 extends App{
  // Вирішіть попередню вправу без застосування циклу.

  val string = StdIn.readLine("Hi! Enter your string:")
  val result = string.map(_.toInt).product

  println(result)
}

object task4 extends App{
  // Напишіть цикл, що міняє місцями суміжні елементи
  // в масиві цілих чисел. Наприклад, Array (1, 2, 3, 4, 5)
  // повинен стати Array (2, 1, 4, 3, 5).

  val input_arr = Array(1, 2, 3, 4, 5, 6)
  val result = {
    for (i <- 1 until input_arr.length if (i % 2) != 0) {
      val first = input_arr(i - 1)
      input_arr(i - 1) = input_arr(i)
      input_arr(i) = first
    }
    input_arr
  }

  println(result.mkString(" "))
}

object task5 extends App{
  // Повторіть попередню вправу, але створіть новий масив
  // з переставленими елементами. Використовуйте вираз for / yield.

  val input_arr = Array(1, 2, 3, 1, 2, 3)
  val result = {
    for (i <- input_arr.indices) yield input_arr(
      if ((i % 2) == 0 && i + 1 == input_arr.length) i
      else if ((i % 2) == 0 ) i + 1
      else i - 1
    )
  }

  println(result.mkString(" "))
}

object task6 extends App{
  // Дан масив цілих чисел, створіть новий масив, в якому
  // спочатку будуть слідувати позитивні значення з оригінального
  // масиву, в оригінальному порядку, а за ними негативні
  // і нульові значення, теж в оригінальному порядку.

  val input_arr = Array(1, -2, 0, 1, 2, -3, -1)
  var index = 0
  val length = input_arr.length
  val result = new Array[Int](length)
  for (i <- 0 until length if input_arr(i) > 0) {
    result(index) = input_arr(i)
    index += 1
  }

  if (index < length) {
    for (i <- 0 until length if input_arr(i) <= 0) {
      result(index) = input_arr(i)
      index += 1
    }
  }

  println(result.mkString(" "))
}

object task7 extends App{
  // Напишіть метод, який повертає значення всіх елементів
  // з масиву, крім повторюваних. Не застосовувати препарат
  // for. (Підказка: загляньте в Scaladoc.)

  val input_arr = Array(1, 1, 1, 3, 1, 1, 2)
  val result = input_arr.distinct

  println(result.mkString(" "))
}

object task8 extends App{
  // Створіть колекцію всіх часових поясів, що повертаються
  // методом java.util.TimeZone.getAvailableIDs. Виберіть елементи
  // для Америки. Відкиньте префікс "America /" і відсортуйте результат.
  // Не застосовувати препарат for.

  val result = {
    TimeZone.getAvailableIDs.filter(_.startsWith("America/"))
    .map(_.stripPrefix("America/")).sorted
  }

  println(result.mkString(" "))
}

object task9 extends App{
  // Створіть асоціативний масив з цінами на речі,
  // які ви хотіли б придбати. Потім створіть другий
  // асоціативний масив з тими ж ключами і цінами з 10% -ою знижкою.

  val dream = Map("one" -> 1000,
    "two" -> 12000,
    "three" -> 20000)
  val result = dream.mapValues(price => price * 0.9)

  println(result.mkString(" "))
}

object task10 extends App{
  // Визначте пов'язану хеш-таблицю, яка буде показувати
  // «Monday» в java.util.Calendar.MONDAY, і так далі для
  // інших днів тижня. Продемонструйте обхід елементів в порядку їх додавання.

  val days = mutable.LinkedHashMap[String, Int]("Monday" -> Calendar.MONDAY)
  days += ("Tuesday" -> Calendar.TUESDAY,
           "Wednesday" -> Calendar.WEDNESDAY,
           "Thursday" -> Calendar.THURSDAY,
           "Friday" -> Calendar.FRIDAY,
           "Saturday" -> Calendar.SATURDAY,
           "Sunday" -> Calendar.SUNDAY)

  println(days)
}

object task11 extends App{
  // Напишіть програму, що читає слова з файлу. Використовуйте
  // змінюваний асоціативний масив для підрахунку входжень кожного слова.

  val file = new java.util.Scanner(getClass.getResourceAsStream(("/test_file.txt")))
  var w_count = MutableMap[String, Int]().withDefault(_ => 0)
  while (file.hasNext()) w_count(file.next.toLowerCase) += 1

  println(w_count)
}

object task12 extends App{
  // Виконайте попередню вправу, використовуючи сортовані
  // асоціативний масив Scala, щоб слова виводилися в відсортованому
  // порядку. І використовуючи java.util.TreeMap,
  // адаптувавши його для роботи зі Scala API.

  val file = new java.util.Scanner(getClass.getResourceAsStream(("/test_file.txt")))
  val w_count: scala.collection.mutable.Map[String, Int] = new java.util.TreeMap[String, Int].asScala
  while (file.hasNext()) {
    val word = file.next.toLowerCase
    w_count += (word -> (w_count.getOrElse(word, 0) + 1))
  }
  println(w_count)
}

object task13 extends App{
  // Виведіть таблицю всіх Java-властивостей

  val props: MutableMap[String, String] = System.getProperties().asScala
  val longestKey = props.keys.foldLeft(0) {
    (cnt, k) => if (k.length > cnt) k.length else cnt
  }
  for ((k, v) <- props)
    println(s"%-${longestKey + 1}s| %s" format(k, v))
}

object task14 extends App{
  // Напишіть функцію lteqgt (values: Array [Int], v: Int),
  // що повертає трійку, яка містить лічильник значень
  // менших v, рівних v і великих v.

  val result = lteqgt(Array(1, 12, -3, 5, 14, 5, 4, -9, 10, 0), 5)

  def lteqgt(values: Array[Int], v: Int): (Int, Int, Int) = {
    var less, equal, more = 0

    for (e <- values) {
      if (e < v) less += 1
      else if (e == v) equal += 1
      else more += 1
    }

    (less, equal, more)
  }

  println(result)
}