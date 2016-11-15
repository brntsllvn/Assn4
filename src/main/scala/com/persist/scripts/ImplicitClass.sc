val l1 = List.empty[Int]
val l2 = List(1, 2, 3)

val a1 = List(l1, l2)
val a2 = a1.map(_.headOption)

// But there is no tailOption


l1.tailOption


trait HasTailOption[T] {
  def tailOption: Option[T]
}


implicit class ExtendList(l: List[Int]) extends HasTailOption[List[Int]] {
  def tailOption = {
    l match {
      case h +: t => Some(t)
      case _ => None
    }
  }
}

l1.tailOption
l2.tailOption
val a3 = a1.map((l: List[Int]) => l.tailOption)

