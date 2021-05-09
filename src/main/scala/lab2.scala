package alokhina.lab1

object task2_1 extends App{
  // Напишіть клас Time з властивостями hours і minutes,
  // доступними тільки для читання, і методом before (other: Time): Boolean,
  // який перевіряє, чи передує час this часу other.
  // Об'єкт Time повинен конструюватися як new Time (hrs, min),
  // де hrs - час в 24-годинному форматі.

  class Time(private val hours: Int, private val minutes: Int) {

    require(hours >= 0 && hours <= 23)
    require(minutes >= 0 && minutes <= 60)

    def before(other: Time): Boolean = {
      hours < other.hours ||
        (hours == other.hours && minutes < other.minutes)
    }
  }

  val t1 = new Time(17, 23)
  println(t1 before new Time(16, 45))
  println(t1 before new Time(20, 15))
}

object task2_1_2 extends App{
  // Зробіть так, щоб усередині час було представлено кількістю хвилин,
  // що минули з початку доби (між 0 і 24 60 - 1).
  // Загальнодоступний інтерфейс при цьому не повинен змінитися.
  // Тобто ці зміни не повинні впливати на клієнтський код.

  class Time(private val minutes: Int) {

    require(minutes >= 0 && minutes <= 24 * 60 - 1)

    def this(hours: Int, minutes: Int) = this((hours * 60) + minutes)

    def before(other: Time): Boolean = minutes < other.minutes
  }

  val t1 = new Time(17, 23)
  println(t1 before new Time(16, 45))
  println(t1 before new Time(20, 15))
}

object task2_2 extends App{
  // Напишіть клас Person з головним конструктором,
  // які приймають рядок, яка містить ім'я,
  // пробіл і прізвище, наприклад: new Person ( «Fred Smith»).
  // Зробіть властивості firstName і lastName доступними тільки для читання.

  class Person(name: String) {
    require(name.split(" +").length == 2)

    val Array(firstName, lastName) = name.split(" +")
  }

  val line = new Person("Mariia Alokhina")
  println(s"firstname: ${line.firstName}, lastname: ${line.lastName} ")
}

object task2_3 extends App{
  // Створіть клас Car з властивостями, що визначають виробника,
  // назву моделі і рік виробництва, які доступні тільки для читання,
  // і властивість з реєстраційним номером автомобіля, доступне для читання/запису.
  // Додайте чотири конструктора. Всі вони повинні приймати назву виробника
  // та назву моделі. При необхідності у виклику конструктора можуть також вказуватися рік
  // і реєстраційний номер. Якщо рік не вказано, він повинен встановлюватися рівним -1,
  // а при відсутності реєстраційного номера повинна встановлюватися порожній рядок.

  class Car(val brand: String,
            val model: String,
            val year: Int,
            var licenseNumber: String) {

    def this(brand: String, model: String) =
      this(brand, model, -1, "")

    def this(brand: String, model: String, year: Int) =
      this(brand, model, year, "")

    def this(brand: String, model: String, licenseNumber: String) =
      this(brand, model, -1, licenseNumber)
  }
//  class Car(val brand: String,
//            val model: String,
//            val year: Int = -1,
//            var licenseNumber: String = "") {}

//    val line = new Car("Mercedes", "G-Class")
//    val line = new Car("Mercedes", "G-Class", 1979)
//    val line = new Car("Mercedes", "G-Class", 1979, "AX300")
    val line = new Car("Mercedes", "G-Class", licenseNumber = "AX300")
  println(s"Brand: ${line.brand}, Name: ${line.model}, Year: ${line.year}, LicenseNumber: ${line.licenseNumber} ")
}

object task2_4 extends App{
  // Визначте для класу Car об'єкт-компаньйон,
  // щоб можна було конструювати екземпляри Car,
  // як Car ( "BMW", "735"), без ключового слова new.

  class Car private(brand: String, year: Int) {}

  object Car {
    def apply(brand: String, year: Int) = new Car(brand, year)
  }

  // Для красивого вывода можно сделать case class
  val result = Car("Mercedes", 1979)
  println(result)
}

object task2_5 extends App{
  // Напишіть Enumeration, яке описує вісім кутів куба RGB.
  // Як числових ідентифікаторів повинні використовуватися
  // значення кольору (наприклад, 0xff0000 - для Red).

  object RGB extends Enumeration {
    val Black   = Value(0x000000)
    val White   = Value(0xffffff)
    val Red     = Value(0xff0000)
    val Green   = Value(0x00ff00)
    val Blue    = Value(0x0000ff)
    val Yellow  = Value(0xffff00)
    val Cyan    = Value(0x00ffff)
    val Magenta = Value(0xff00ff)
  }

  println(RGB.Magenta)
  println("0x%x".format(RGB.Cyan.id))
}

object task2_6 extends App{
  // Напишіть програму, що копіює всі елементи з Java-хешу в Scala-хеш.
  // Використовуйте операцію імпортування для перейменування обох класів.

  import java.util.{HashMap => JavaHashMap}
  import scala.collection.mutable.{HashMap => ScalaHashMap}

  def fromJava[K, V](javamap: JavaHashMap[K, V]): ScalaHashMap[K, V] = {
    var result = new ScalaHashMap[K, V]()
    var iter = javamap.entrySet().iterator()
    while (iter.hasNext) {
      val e = iter.next
      result(e.getKey) = e.getValue
    }

    result
  }

  var map = new JavaHashMap[String, Int]()
  map.put("a", 1)
  map.put("b", 2)
  map.put("c", 3)

  println(fromJava(map))
}

object task2_7 extends App{
  // Напишіть програму, що імпортує клас java.lang.System,
  // читає ім'я користувача з системного властивості user.name,
  // пароль з об'єкта Console і виводить повідомлення в стандартний потік помилок,
  // якщо пароль недостатньо «секретний». В іншому випадку програма повинна вивести
  // вітання в стандартний потік виведення. Чи не імпортуйте нічого іншого
  // і не використовуйте повні кваліфіковані імена (з точками).

  import java.lang.System._
  import scala.io.StdIn.readLine

  val CORRECT_PASS = "secret"
  val userName = getProperty("user.name")
  val password: String = readLine("Please, enter password: ")

  if (password == CORRECT_PASS) { out.println("hello " + userName)
  } else { err.println("Wrong password") }
}

object task2_8 extends App{
  // Визначте клас CheckingAccount, що успадковує клас BankAccount,
  // який стягує $ 1 комісійних за кожну операцію поповнення або списання.

  class BankAccount(initialBalance: Double) {
    private var _balance = initialBalance

    def deposit(amount: Double) = { _balance += amount; _balance }

    def withdraw(amount: Double) = { _balance -= amount; _balance }

    def balance() = _balance

    override def toString: String = s"Account{balance = ${_balance}}"
  }

  class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
    private val charges: Double = 1

    override def deposit(amount: Double): Double = { super.deposit(amount - charges) }
    override def withdraw(amount: Double): Double = { super.withdraw(amount + charges) }
  }

  val account = new CheckingAccount(100)
//  account.deposit(5)
//  account.withdraw(10)

  println(account.deposit(5))
}

object task2_9 extends App{
  // Визначте клас SavingsAccount, що успадковує клас BankAccount
  // з попередньої вправи, який нараховує відсотки щомісяця(викликом методу earnMonthlyInterest)
  // і дозволяє безкоштовно виконувати три операції зарахування
  // або списання кожного місяця. Метод earnMonthlyInterest повинен скидати лічильник транзакцій.
  class BankAccount(initialBalance: Double) {
    private var _balance = initialBalance

    def deposit(amount: Double) = { _balance += amount; _balance }

    def withdraw(amount: Double) = { _balance -= amount; _balance }

    def balance() = _balance

    override def toString: String = s"Account{balance = ${_balance}}"
  }

  class SavingsAccount(initialBalance: Double,
                       charges: Double,
                       val interest: Double) extends BankAccount(initialBalance) {
    require(interest >= 0 && interest <= 1)

    val max_free = 3
    private[this] var in_month = 0

    def earnMonthlyInterest() = {
      in_month = 0
      super.deposit(balance * interest)
    }
    override def deposit(amount: Double): Double = {
      in_month += 1
      super.deposit(amount - (if (in_month > max_free) charges else 0))
    }
    override def withdraw(amount: Double): Double = {
      in_month += 1
      super.withdraw(amount + (if (in_month > max_free) charges else 0))
    }
  }
  val account = new SavingsAccount(100, 5, 0.1)
  account.deposit(10)
  account.withdraw(20)
  account.earnMonthlyInterest()
  println(account)
}

object task2_10 extends App{
  // Визначте абстрактний клас геометричної фігури Shape
  // з абстрактним методом centerPoint і підкласи прямокутника і кола,
  // Rectangle і Circle. Реалізуйте відповідні конструктори в підкласах
  // і перевизначите метод centerPoint в кожному підкласі.

  class Point(val x: Double, val y: Double)

  abstract class Shape { def centerPoint: Point }

  class Circle(override val centerPoint: Point, val radius: Double) extends Shape

  class Rectangle(val x1: Double, val y1: Double, val x2: Double, val y2: Double) extends Shape {
    override def centerPoint = new Point((x1 + x2) / 2, (y1 + y2) / 2)
  }

  val circle: Shape = new Circle(new Point(1, 2), 3)
  val circleCenter: Point = circle.centerPoint
  println(circleCenter.x)
  println(circleCenter.y)

  val rectangle: Shape = new Rectangle(1, 2, 3, 4)
  val rectangleCenter: Point = rectangle.centerPoint
  println(rectangleCenter.x)
  println(rectangleCenter.y)
}
