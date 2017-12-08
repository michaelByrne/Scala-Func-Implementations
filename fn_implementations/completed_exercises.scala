package fn_implementations

import fpinscala.datastructures.List.{sum, product}
import fpinscala.datastructures.Cons
import fpinscala.datastructures.List



object completed_exercises {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Int =
      if (n >= as.length - 1) -1
      else if (!ordered(as(n), as(n + 1))) 1
      else loop(n + 1)

    if (loop(0) < 0) return true
    false
  }

  def increasingOrderPair(first: Int, second: Int): Boolean = {
    if (first >= second) return false
    true
  }

  // 3.1
  val derp = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

}
