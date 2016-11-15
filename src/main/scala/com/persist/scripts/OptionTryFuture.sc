import scala.concurrent.{Await, Future}
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


val o1 = Option[Int](10)
val o2 = None
val s = List(o1, o2)

val s1 = s.map(_.map(_ + 1))

def chain(o: Option[Int]) = {
  o.map(100 / _).map(2 * _)
}
val sc = s.map(chain)

val s2 = for {
  sx <- s;
  ox <- sx
} yield (ox + 1)


def e: Int = throw new Exception("foo")
val t1 = Try(10)
val t2 = Try(e)
val t3 = Try(0)
val t = List(t1, t2, t3)

val ta = t.map(_.map(_ + 1))

def chaint(o: Try[Int]) = {
  o.map(100 / _).map(2 * _)
}
val tc = t.map(chaint)


def fget[T](f: Future[T]) = {
  Await.result(f, Duration.Inf)
}
val f1 = Future(10 + 1)
val f2 = Future(20 * 3)
val fv1 = f1.map(_ + 1).map(_ * 2)
val v1 = fget(fv1)
val fv2 = for {
  v1 <- f1;
  v2 <- f2.map(_ + v1)
} yield (v1, v2)

val v2 = fget(fv2)
