val s1 = List(1, 2, 3, 23, -1)

val s2 = s1.map(_ + 1)

val s3 = s1.filter(_ > 2)

val s4 = s1.map(_.toString)

val s5 = s1.flatMap(_.toString)

def map1(s: List[Int], f: Int => Int): List[Int] = {
  s.flatMap(i => List(f(i)))
}

val s6 = map1(s1, _ + 1)

def filter1(s: List[Int], f: Int => Boolean): List[Int] = {
  s.flatMap(i => if (f(i)) List(i) else List.empty[Int])
}

val s7 = filter1(s1, _ > 2)
