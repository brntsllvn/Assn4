val s1 = List(1, 2, 4, 7)
val s2 = List(3, 4, 2)

val p = s1 zip s2

val si = s1.zipWithIndex

def asc(s: List[Int]) = {
  (s zip s.tail).forall {
    case (a, b) => a <= b
  }
}

val b1 = asc(s1)

val b2 = asc(s2)

val s3 = s2.sortBy(i => i)

val s4 = s2.sortBy(i => -i)

val g1 = s1.groupBy(_ % 2)

val t = "this is a test of character frequency"

val ch = t.groupBy(ch => ch).map {
  case (ch, s) => (s"'$ch'", s.length)
}

val ch1 = ch.toSeq.sortBy(p => -p._2)