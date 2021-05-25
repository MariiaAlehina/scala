package alokhina.lab1

object task3_1 extends App{
  // Використовуючи зіставлення зі зразком, напишіть функцію swap,
  // яка приймає пару цілих чисел і повертає ту ж пару,
  // помінявши компоненти місцями.

  def swap(p: (Int, Int)): (Int, Int) = p match { case (x, y) => (y, x) }
  // ту же пару невозможно вернуть
  println(swap((5, 6)))
}

object task3_2 extends App{
  // Використовуючи зіставлення зі зразком, напишіть функцію swap,
  // яка міняє місцями перші два елементи масиву,
  // якщо він має довжину не менше двох.

  def swap(arr: Array[Int]): Array[Int] = arr match {
    case Array(x, y, _*) =>
      arr(0) = y
      arr(1) = x
      arr
    case _ => arr
  }

  println(swap(Array(1, 2, 3)).mkString(" "))
}

object task3_3 extends App{
  // Для подання дерев, що зберігають значення тільки в листі,
  // можна використовувати списки. Наприклад, список ((3 8) 2 (5)) описує дерево
  //      •
  //    / | \
  //   •  2  •
  //  / \    |
  //  3 8    5
  // В цьому випадку одні елементи списку будуть числами, а інші - списками.
  // Однак в Scala можна створювати різнорідні списки, тому доведеться використовувати List [Any].
  // Напишіть функцію leafSum для обчислення суми всіх значень листя,
  // використовуючи зіставлення зі зразком для відділення чисел від списків.


  def leafSum(tree: List[Any]): Int = tree match {
    case (head: Int) :: tail => head + leafSum(tail)
    case (head@List(_*)) :: tail => leafSum(head) + leafSum(tail)
    case Nil => 0
  }

  val tree = List(List(3, 8), 2, List(5))
  println(leafSum(tree))
}

object task3_4 extends App{
  // Дерева найкраще моделювати із застосуванням case-класів. Почніть з бінарних дерев.
  // sealed abstract class BinaryTree
  // case class Leaf (value: Int) extends BinaryTree
  // case class Node (left: BinaryTree, right: BinaryTree) extends BinaryTree
  // Напишіть функцію, яка обчислює суму всіх значень листя.

  sealed abstract class BinaryTree

  case class Leaf(value: Int) extends BinaryTree

  case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

  def leafSum(bt: BinaryTree): Int = bt match {
    case leaf: Leaf => leaf.value
    case left Node right => leafSum(left) + leafSum(right)
  }

  val tree = Node(Node(Leaf(3), Leaf(8)), Node(Leaf(2), Leaf(5)))
  println(leafSum(tree))
}

object task3_5 extends App{
  // Розширте дерево з попередньої вправи, щоб кожен вузол в ньому міг мати довільну
  // кількість дочірніх вузлів, і перепишіть функцію leafSum. Дерево в завданні 3 має виражатися як:
  // Node (Node (Leaf (3), Leaf (8)), Leaf (2), Node (Leaf (5)))

  sealed abstract class BinaryTree

  case class Leaf(value: Int) extends BinaryTree

  case class Node(children: BinaryTree*) extends BinaryTree

  def leafSum(bt: BinaryTree): Int = bt match {
    case leaf: Leaf => leaf.value
    case node: Node => node.children.foldLeft(0)((acc, item) => acc + leafSum(item))
  }

  val tree = Node(Node(Leaf(3), Leaf(8), Leaf(1)), Node(Leaf(2), Leaf(5), Leaf(2)))
  println(leafSum(tree))
}

object task3_6 extends App{
  // Розширте дерево з попередньої вправи, щоб кожен вузол, який не є листом,
  // на додачу до дочірнім вузлів міг зберігати оператор. Потім напишіть функцію eval,
  // яка обчислює значення. Наприклад, дерево
  //    +
  //  / | \
  //  * 2 -
  // / \  |
  // 3 8  5 має значення (3 8) + 2 + (-5) = 21

  sealed abstract class BinaryTree

  case class Leaf(value: Int) extends BinaryTree

  case class Node(op: Op, children: BinaryTree*) extends BinaryTree

  class Op private(val identity: Int, op: (Int, Int) => Int) {
    def apply(a: Int, b: Int): Int = op(a, b)
  }

  object Op {
    val Plus = new Op(0, _ + _)
    val Minus = new Op(0, _ - _)
    val Product = new Op(1, _ * _)
  }

  def eval(bt: BinaryTree): Int = bt match {
    case leaf: Leaf => leaf.value
    case node: Node => node.children.foldLeft(node.op.identity) { (acc, item) =>
      node.op(acc, eval(item))
    }
  }

  val tree = Node(Op.Plus, Node(Op.Product, Leaf(3), Leaf(8)), Leaf(2), Node(Op.Minus, Leaf(5)))
  println(eval(tree))
}

object task3_7 extends App{
  // Визначте незмінний клас Pair [T, S] з методом swap,
  // що повертає нову пару, де компоненти поміняні місцями.

  class Pair[T, S](val first: T, val second: S) {
    def swap(): Pair[S, T] = new Pair(second, first)
    override def toString = "" + (first, second)
  }

  val pair = new Pair(1, "2")
  println(pair.swap())
}

object task3_8 extends App{
  // Визначте змінюваний клас Pair [T] з методом swap,
  // який змінює компоненти пари місцями.

  class Pair[T](var first: T, var second: T) {
    def swap() {
      val tmp = first
      first = second
      second = tmp
    }
    override def toString = "" + (first, second)
  }

  val pair: Pair[Int] = new Pair(1, 2)
  pair.swap()
  println(pair)
}

object task3_9 extends App{
  // Для класу Pair [T, S] напишіть узагальнений метод swap,
  // який приймає пару у вигляді аргументу і повертає нову
  // пару з компонентами, помінялися місцями.
  class Pair[T, S](val first: T, val second: S) {
    override def toString = "" + (first, second)
  }

  def swap[T, S](pair: Pair[T, S]): Pair[S, T] =
    new Pair(pair.second, pair.first)

  val pair = new Pair(1, 2)
  println(swap(pair))
}

object task3_10 extends App{
  // Напишіть узагальнений метод middle, який повертає середній елемент
  // з будь-якого примірника Iterable [T].
  // Наприклад, виклик middle ( "World") повинен повернути 'r'.

  def middle[A, C](xs: C)(implicit ev: C => Iterable[A]): Option[A] = {
    val size = xs.size
    if (size % 2 == 0) {
      return None
    }

    var distance = size / 2
    xs.find { _ =>
      val found = if (distance == 0) true else false
      distance -= 1
      found
    }
  }
  println(middle("World"))
  println(middle("ALokALOK"))
}

object task3_11 extends App{
  // Для змінюваного класу Pair [S, T] використовуйте механізм обмеження типу,
  // щоб визначити метод swap, який можна викликати з параметрами одного типу.

  class Pair[S, T](var first: S, var second: T) {

    def swap(implicit ev: T =:= S): Unit = {
      val tmp = first.asInstanceOf[T]
      first = second
      second = tmp
    }
  }

  val pair = new Pair(1.0, 2.0)
  pair.swap
  println(pair.first)
  println(pair.second)
}
