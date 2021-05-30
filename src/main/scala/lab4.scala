package alokhina.lab1

import alokhina.lab1.task4_8.result

import java.io
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import java.io.{BufferedInputStream, FilterInputStream, InputStream}

object task4_1 extends App{
  // У бібліотеці java.io є можливість додати буферизацию в потік
  // введення за допомогою декоратора BufferedInputStream. Реалізуйте
  // буферизацию як трейт. Для простоти перевизначте метод read

  trait BufferedInputStreamLike {
    this: InputStream => val InputStream = new BufferedInputStream(this)

    override def read(ab: Array[Byte]): Int = {
      InputStream.read(ab)
    }
  }

  val f_is = new FilterInputStream(getClass.getResourceAsStream("/test_file.txt")) with BufferedInputStreamLike
  val bytes = new Array[Byte](1 << 4)
  f_is.read(bytes)
  bytes.foreach(byte => print(byte.toChar))
}

object task4_2 extends App{
  // Реалізуйте клас Fraction з операціями + - * /.
  // Реалізуйте нормалізацію раціональних чисел,
  // наприклад щоб число 15 / -6 перетворювалося в -5/3,
  // а також розподіл на найбільший спільний дільник

  class Fraction private(n: Int, d: Int) {

    val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)
    val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)

    private def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0

    private def gcd(a: Int, b: Int): Int = if (b == 0) Math.abs(a) else gcd(b, a % b)

    def +(other: Fraction): Fraction = Fraction(num * other.num + other.num * num, den * other.den)
    def -(other: Fraction): Fraction = Fraction(num * other.num - other.num * num, den * other.den)
    def *(other: Fraction): Fraction = Fraction(num * other.num, den * other.den)
    def /(other: Fraction): Fraction = Fraction(num * other.den, den * other.num)

    override def equals(that: Any): Boolean = that match {
      case that: Fraction => num == that.num && den == that.den
      case _ => false
    }
    override def toString = num + "/" + den
  }

  object Fraction {
    def apply(n: Int, d: Int) = new Fraction(n, d)
  }

  println(Fraction(15, -6).toString )
  println((Fraction(1, 2) + Fraction(1, 2)).toString)
  println((Fraction(1, 2) - Fraction(1, 2)).toString)
  println((Fraction(1, 2) * Fraction(1, 2)).toString)
  println((Fraction(1, 2) / Fraction(1, 2)).toString)
  println(Fraction(15, -6) == Fraction(15, -6))
  println(Fraction(15, 6) == Fraction(-15, 6))
  println(Fraction(15, 6) == null)
}

object task4_3 extends App{
  // Реалізуйте клас Money з полями для вираження суми в доларах і центах.
  // Реалізуйте оператори +, -, а також оператори порівняння == і <.
  // Наприклад, вираз Money (1, 75) + + Money (0, 50) == Money (2, 25) має повертати true.

  class Money private(d: Int, c: Int) {
    val dollars: Int = if (c > 99) d + (c / 100) else d
    val cents: Int = if (c > 99) c % 100 else c

    def +(other: Money): Money = Money(dollars + other.dollars, cents + other.cents)

    def -(other: Money): Money = {
      val d: Int = dollars - other.dollars
      val c: Int = cents - other.cents
      if (c < 0) Money(d - 1, c + 100)
      else Money(d, c)
    }

    def ==(other: Money): Boolean = dollars == other.dollars && cents == other.cents

    def <(other: Money): Boolean = (dollars * 100) + cents < (other.dollars * 100) + other.cents

    override def toString = "%d.%02d".format(dollars, + cents)
  }

  object Money {
    def apply(d: Int, c: Int) = new Money(d, c)
  }

  println(Money(1, 150).toString)
  println((Money(1, 75) + Money(0, 50)).toString)
  println((Money(2, 25) - Money(0, 50)).toString)
  println(Money(2, 25) == Money(0, 50))
  println(Money(2, 25) == Money(2, 25))
  println(Money(1, 50) < Money(2, 25))
}

object task4_4 extends App{
  // Реалізуйте оператори конструювання HTML-таблиці. Наприклад, вираз
  // Table () | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET"
  // має повертати
  // <Table> <tr> <td> Java </ td> <td> Scala </ td> </ tr> <tr> <td> Gosling

  class Table private {

    private val table = new ListBuffer[ListBuffer[String]]

    def |(cell: String): Table = {
      if (table.isEmpty) { table += new ListBuffer[String] }
      table.last += cell
      this
    }

    def ||(cell: String): Table = {
      table += new ListBuffer[String]
      table.last += cell
      this
    }

    def toHtml: String = {
      val sb = new StringBuilder("<table>")
      for (row <- table) {
        sb ++= "<tr>"
        for (cell <- row) {
          sb ++= "<td>"
          sb ++= cell
          sb ++= "</td>"
        }
        sb ++= "</tr>"
      }

      sb ++= "</table>"
      sb.toString()
    }

    override def toString = table.mkString("\n")
  }

  object Table {
    def apply() = new Table
  }

  val table = Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET"

  println(table.toHtml)
}

object task4_5 extends App{
  // Реалізуйте клас Matrix - розміру m × n. Реалізуйте операції + і *.
  // Остання повинна також дозволяти виконувати множення на скаляр, наприклад mat * 2.
  // Єдиний елемент матриці повинен бути доступний як mat (row, col).

  class Matrix private(private val rows: Int,
                       private val cols: Int,
                       private val data: IndexedSeq[Int]) {

    def +(other: Matrix): Matrix = {
      val thisSize = size
      if (thisSize != other.size) {
        throw new IllegalArgumentException("matrices are not of the same size" +
          ", expected: " + thisSize + ", actual: " + other.size)
      }

      Matrix.init(rows, cols)((row, col) => this(row, col) + other(row, col))
    }

    def *(value: Int): Matrix =
      Matrix.init(rows, cols)((row, col) => this(row, col) * value)

    def *(other: Matrix): Matrix = {
      val (m1, n1) = size
      val (m2, n2) = other.size
      if (n1 != m2) {
        throw new IllegalArgumentException("matrices are not multiplication compatible" +
          ", expected rows: " + n1 + ", actual: " + m2)
      }

      Matrix.init(m1, n2) { (row, col) => (0 until n1).foldLeft(0)((sum, i) =>
        sum + data(row * n1 + i) * other.data(i * n2 + col))
      }
    }

    def apply(row: Int, col: Int): Int = data(row * cols + col)

    def size: (Int, Int) = (rows, cols)

    override def toString = {
      val sb = new StringBuilder
      for (row <- 0 until rows) {
        if (row > 0) {
          sb ++= "\n"
        }

        sb ++= "["
        for (col <- 0 until cols) {
          if (col > 0) {
            sb ++= ", "
          }

          sb.append(data(row * cols + col))
        }
        sb ++= "]"
      }

      sb.toString()
    }
  }

  object Matrix {
    def apply(rows: Int, cols: Int)(data: Int*): Matrix = {
      if ((rows * cols) != data.length) {
        throw new IllegalArgumentException("(" + rows + " x " + cols +
          ") data length: " + data.length)
      }

      new Matrix(rows, cols, data.toIndexedSeq)
    }

    private def init(rows: Int, cols: Int)(op: (Int, Int) => Int): Matrix = {
      val data = new Array[Int](rows * cols)
      for (row <- 0 until rows) {
        for (col <- 0 until cols) {
          data(row * cols + col) = op(row, col)
        }
      }

      new Matrix(rows, cols, data)
    }
  }

  val result = Matrix(2, 2)(
    1, 2,
    3, 4) +
    Matrix(2, 2)(
      5, 6,
      7, 8)

  println(result(0, 0))
  println(result.toString)
}

object task4_6 extends App{
  // Напишіть функцію values values(low: Int, high: Int, fun: (Int) => Int),
  // що повертає колекцію зі значень у вказаному діапазоні. Наприклад,
  // виклик values values(-5, 5, x => x * x) повинен повернути колекцію
  // пар (-5, 25), (-4, 16), (-3, 9), ..., (5, 25 ).

  def values(fun: (Int) => Int, low: Int, high: Int): Seq[(Int, Int)] = {
    for (i <- low to high) yield (i, fun(i))
  }

  val result: Seq[(Int, Int)] = values(x => x * x, -5, 5)
  println(result)
}

object task4_7 extends App{
  // Реалізуйте попереднє завдання використовуючи каррінг та неявний параметр:
  // values(l: Int, h: Int)(implicit f: Int => Int)

  def values(low: Int, high: Int)(implicit fun: Int => Int): Seq[(Int, Int)] = {
    for (i <- low to high) yield (i, fun(i))
  }

  println(values(-5, 5)(x => x * x))
}

object task4_8 extends App{
  // Напишіть функцію largest (inputs: Seq [Int], fun: (Int) => Int),
  // що повертає найбільше значення функції всередині заданої послідовності.
  // Наприклад, виклик largest (1 to 10, x => 10 * x - - x * x) повинен повернути 25.
  // Не використовуйте цикл або рекурсію.

  def largest(fun: (Int) => Int, inputs: Seq[Int]): Int = {
    inputs.map(fun).max
  }

  val result: Int = largest(x => 10 * x - x * x, 1 to 10)
  println(result)
}

object task4_9 extends App{
  // Реалізуйте попереднє завдання використовуючи каррінг
  // та неявний параметр: largest (1 to 10)(x => 10 * x - - x * x)

  def largest(inputs: Seq[Int])(implicit fun: (Int) => Int): Int = {
    inputs.map(fun).max
  }

  println(largest( 1 to 10)(x => 10 * x - x * x))
}

object task4_10 extends App{
  // Напишіть функцію, яка повертає для зазначеного рядка відображення
  // індексів усіх символів. Наприклад, виклик indexes ("Mississippi")
  // повинен повернути асоціативний масив, що зв'язує 'M' з безліччю {0}, 'i' -
  // з безліччю {1, 4, 7, 10} і т.д. Використовуйте змінюваний асоціативний масив,
  // що відображає символи в змінювані безлічі.

  def indexes(s: String): mutable.Map[Char, mutable.Set[Int]] = {
    val map = new mutable.HashMap[Char, mutable.Set[Int]]
    for (i <- 0 until s.length) {
      map.getOrElseUpdate(s(i), new mutable.LinkedHashSet[Int]) += i
    }
    map
  }

  val result: mutable.Map[Char, mutable.Set[Int]] = indexes("Mississippi")
  println(result('M'))
  println(result('i'))
}

object task4_11 extends App{
  // Напишіть функцію, що видаляє обрані елементи з пов'язаного списку цілих чисел.
  // Функція повинна приймати предикат як другий параметр.

  def removeElements(list: List[Int], p: Int => Boolean): List[Int] = {
    val inverse = (i: Int) => !p(i)
    list.filter(inverse)
  }

  println(removeElements(List(6, 5, 4, 3, 2, 1), x => x % 2== 0))
}
