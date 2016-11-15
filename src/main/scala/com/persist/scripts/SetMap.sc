val s = Set(1, 2, 3, 4, 5)

val s1 = s.map(_ / 2)

val m = Map(1 -> "one", 2 -> "two", 3 -> "three")

val m1 = for {
  (k, v) <- m
} yield (v, k)