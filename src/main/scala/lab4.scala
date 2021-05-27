package alokhina.lab1

import alokhina.lab1.task4_8.result

import scala.collection.{mutable}
import scala.collection.mutable.{ListBuffer}
import scala.io.Source
import java.io.{FilterInputStream, InputStream}

object task4_1 extends App{
  // У бібліотеці java.io є можливість додати буферизацию в потік
  // введення за допомогою декоратора BufferedInputStream. Реалізуйте
  // буферизацию як трейт. Для простоти перевизначте метод read

  trait Logger { def log(msg: String): Unit  }

  trait ConsoleLogger extends Logger {
    override def log(msg: String): Unit = println(msg)
  }

  trait BufferedInputStreamLike extends InputStream with Logger {
    val bufferSize = 2048
    private val buffer = new Array[Byte](bufferSize)
    private var offset: Int = 0
    private var size: Int = 0

    abstract override def read(): Int = {
      if (size == -1) {
        return -1
      }

      if (offset >= size) {
        offset = 0
        size = 0

        log("Filling the buffer, bufferSize: " + bufferSize)
        fillBuffer(0)

        if (size == 0) {
          log("Reached stream end")
          size = -1
          return -1
        }

        log("Buffer is filled, size: " + size)
      }

      val byte = buffer(offset)
      offset += 1
      byte
    }

    private def fillBuffer(index: Int): Unit = {
      if (index >= buffer.length) {
        return
      }

      val byte = super.read()
      if (byte == -1) {
        return
      }

      buffer(index) = byte.toByte
      size += 1
      fillBuffer(index + 1)
    }
  }

  val in = new {
    override val bufferSize = 48
  } with FilterInputStream(getClass.getResourceAsStream("/myfile.txt"))
    with BufferedInputStreamLike
    with ConsoleLogger

//  val result = Source.fromBytes(readBytes(in)).mkString
}

object task4_2 extends App{
  // Реалізуйте клас Fraction з операціями + - * /.
  // Реалізуйте нормалізацію раціональних чисел,
  // наприклад щоб число 15 / -6 перетворювалося в -5/3,
  // а також розподіл на найбільший спільний дільник

  class Fraction private(n: Int, d: Int) {

    val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)

    val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)

    private def sign(a: Int) =
      if (a > 0) 1
      else if (a < 0) -1
      else 0

    private def gcd(a: Int, b: Int): Int =
      if (b == 0) Math.abs(a)
      else gcd(b, a % b)

    def +(other: Fraction): Fraction = sumOp(other, _ + _)

    def -(other: Fraction): Fraction = sumOp(other, _ - _)

    def *(other: Fraction): Fraction = Fraction(num * other.num, den * other.den)

    def /(other: Fraction): Fraction = Fraction(num * other.den, den * other.num)

    private def sumOp(other: Fraction, op: (Int, Int) => Int): Fraction = {
      Fraction(op(num * other.den, den * other.num), den * other.den)
    }

    override def equals(that: Any): Boolean = that match {
      case that: Fraction => num == that.num && den == that.den
      case _ => false
    }

    override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + num
      result = prime * result + den
      result
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

    if (d < 0) throw new IllegalArgumentException("d: " + d)
    if (c < 0) throw new IllegalArgumentException("c: " + c)

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
      if (table.isEmpty) {
        table += new ListBuffer[String]
      }

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
}

object task4_8 extends App{
  // Напишіть функцію largest (inputs: Seq [Int], fun: (Int) => Int),
  // що повертає найбільше значення функції всередині заданої послідовності.
  // Наприклад, виклик largest (1 to 10, x => 10 * x - - x * x) повинен повернути 25.
  // Не використовуйте цикл або рекурсію.

  def largest(fun: (Int) => Int, inputs: Seq[Int]): Int = {
    inputs.map(fun(_)).reduceLeft((a, b) => if (a > b) a else b)
  }

  val result: Int = largest(x => 10 * x - x * x, 1 to 10)
  println(result)
}

object task4_9 extends App{
  // Реалізуйте попереднє завдання використовуючи каррінг
  // та неявний параметр: largest (1 to 10)(x => 10 * x - - x * x)
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
}