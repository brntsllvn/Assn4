package com.persist.uw.examples

case class State(contains: List[Int]) {
  override def toString = contains.mkString("[", ",", "]")
}

case class Water(end: State, sizes: State) {
  type Path = List[State]

  def solve(start: State): Option[Path] = solve1(Set(List(start)), Set.empty[State])

  def move(state: State, from: Int, to: Int): State = {
    val fromHas = state.contains(from)
    val toHas = state.contains(to)
    val toSize = sizes.contains(to)
    val toFree = toSize - toHas
    val pour = if (toFree < fromHas) toFree else fromHas
    State(state.contains.zipWithIndex map {
      case (has, i) if i == from => has - pour
      case (has, i) if i == to => has + pour
      case (has, i) => has
    })
  }

  def step(state: State): Set[State] = {
    val all = state.contains zip sizes.contains zipWithIndex
    val x = for {
      ((fromHas, fromSize), from) <- all if fromHas > 0
      ((toHas, toSize), to) <- all if from != to && toHas < toSize
    } yield move(state, from, to)
    x.toSet
  }

  def solveStep(start: Set[Path], known: Set[State]): Set[Path] = {
    start.headOption match {
      case Some(p) =>
        val newStates = step(p.head).filter(!known.contains(_))
        val next = newStates.map(_ +: p)
        if (newStates.contains(end)) {
          next
        } else {
          val s2 = solveStep(start - p, known ++ newStates)
          next ++ s2
        }
      case None => Set.empty
    }
  }

  def solve1(start: Set[Path], known: Set[State]): Option[Path] = {
    val next = solveStep(start, known)
    val newStates = next.map(_.head)
    if (newStates.contains(end)) {
      Some(next.filter(_.head == end).head.reverse)
    } else if (newStates.isEmpty) {
      None
    } else {
      solve1(next, known union newStates)
    }
  }
}

object WaterTest {

  case class Jar(size: Int, start: Int, end: Int)

  def test(name: String, jars: List[Jar]): Unit = {
    val sizes = State(jars.map(_.size))
    val start = State(jars.map(_.start))
    val end = State(jars.map(_.end))
    val water = Water(end, sizes)
    val result = water.solve(start)
    println(s"*** $name $sizes ***")
    result match {
      case None => println("No Solution")
      case Some(r) =>
        if (r.size > 25) {
          println(s"Too many steps ${r.size}")
        } else
          for ((s, i) <- r.zipWithIndex) println(s"${i + 1} $s")
    }
  }

  def main(args: Array[String]): Unit = {
    val jars1 = List(Jar(3, 0, 0), Jar(5, 0, 4), Jar(8, 8, 4))
    val jars2 = List(Jar(5, 0, 0), Jar(11, 0, 8), Jar(13, 0, 8), Jar(24, 24, 8))
    val jars3 = List(Jar(4, 0, 3), Jar(5, 0, 3), Jar(10, 10, 4), Jar(10, 10, 10))
    val jars4 = List(Jar(2, 0, 2), Jar(2, 0, 2), Jar(100, 0, 48), Jar(100, 100, 48))
    val jars5 = List(Jar(2, 0, 1), Jar(2, 0, 1), Jar(100, 0, 49), Jar(100, 100, 49))
    test("Test1", jars1)
    test("Test2", jars2)
    test("Test3", jars3)
    test("Test4", jars4)
    test("Test5", jars5)
  }
}
