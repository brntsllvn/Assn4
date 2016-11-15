import scala.collection.immutable._

val s = Seq.empty[Int]
val s1 = Seq(1, 3, 10)
val s2 = List(1, 3, 10)
val s3 = Vector(1, 3, 10)

val t = Set.empty[Int]
val t1 = Set(1, 3, 10)
val t2 = HashSet(1, 3, 10)
val t3 = TreeSet(1, 3, 10)
val t4 = BitSet(1, 3, 10)

val m = Map.empty[Int, String]
val m1 = Map(1 -> "foo", 2 -> "bar")
val m2 = HashMap(1 -> "foo", 2 -> "bar")
val m3 = TreeMap(1 -> "foo", 2 -> "bar")

val ta = s1.toSet
val sa = t1.toList
val sb = m1.toList
val tb = m1.toSet
val ma = tb.toMap
