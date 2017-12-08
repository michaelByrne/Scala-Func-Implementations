package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[T](ys: List[T]): List[T] = ys match {
    case Nil => Nil
    case Cons(y,ys) => ys
  }

  def setHead[T](h: T, hs: List[T]): List[T] = hs match {
    case Nil => List(h)
    case Cons(j, hs) => Cons(h, hs)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case i if i < 0 => ???
    case _ => drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t,f)
    case _ => l
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
    as match{
      case Nil => z
      case Cons(x,xs) => f(x, foldRight(xs,z)(f))
    }
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    def loop(as: List[A], acc: B): B = as match {
      case Nil => acc
      case Cons(x,xs) => loop(xs, f(acc,x))
    }
    loop(as,z)
  }

}
