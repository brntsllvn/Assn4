val s = List(1, 3, 18, -6, 3)

val s1 = s.map(_ + 1)

val s2 = for (i <- s) yield (i + 1)

val s3 = s.filter(_ % 2 == 0).map(_ + 1)

val s4 = for (i <- s if i % 2 == 0) yield (i + 1)

val s5 = for {
  i <- 1 to 3;
  j <- i to 3
} yield (i, j)

val s6 = (1 to 3).map {
  i => (i to 3).map {
    j => (i, j)
  }
}

val s7 = (1 to 3).flatMap {
  i => (i to 3).map {
    j => (i, j)
  }
}

val s8 = (1 to 3).toList.flatMap {
  i => (i to 3).toVector.map {
    j => (i, j)
  }
}

val s9 = (1 to 3).toVector.flatMap {
  i => (i to 3).toList.map {
    j => (i, j)
  }
}

for (i <- 1 to 2) println(i)

(1 to 2).foreach(println(_))