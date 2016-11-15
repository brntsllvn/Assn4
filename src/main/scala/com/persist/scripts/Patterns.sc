val p = (2, "abc")
val (cnt, name) = p

val s = List((1, 2), (3, 5), (7, 4))

val s1 = s.map(p => p._1 + p._2)

val s2 = s.map { case (a, b) => a + b }

val s3 = for ((a, b) <- s) yield a + b

val t = List(List(), List(1, 2, 3), List(4, 5), List())

val heads = t.flatMap {
  case h +: t => Seq(h)
  case _ => Seq()
}

val fruits = List("100 bananas", "3 pears", "6 apples", "5 pears")

val pattern = "([0-9]+) ([A-Za-z]+)".r

val fruits1 = fruits.map {
  case pattern(cnt, fruit) => (cnt.toInt, fruit)
  case _ => (0, "???")
}

val fruits2 = fruits1.groupBy { case (cnt, f) => f }

val fruits3 = fruits2.map { case (f, list) =>
  (f, list.map { case (cnt, f1) => cnt }.sum)
}
