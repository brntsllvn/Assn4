val s = List(1, 2, 3)

val sum = s.fold(0)(_ + _)

// 0 + 1 + 2 + 3
// eval order not defined
// identify:0 associate op: +
// result type == element type

val max = s.fold(0)((a, b) => if (a > b) a else b)

val sum1 = s.foldLeft(0)(_ + _)
// ((0 + 1) + 2) + 3
// op need not be associative
// result type need not == element type

val rev = s.foldLeft(List.empty[Int])((s, i) => i +: s)

// 1 :+ (2:+ (3:+ () ))

val fwd = s.foldRight(List.empty[Int])((i, s) => i +: i +: s)

val s1 = List(List(1, 2), List(3, 4), List(6))

s1.flatMap(s => s)

def flatMap1(s: List[List[Int]], f: List[Int] => List[Int]) = {
  s.foldLeft(List.empty[Int]) {
    (a, b) => a ++ b
  }
}

flatMap1(s1, s => s)

