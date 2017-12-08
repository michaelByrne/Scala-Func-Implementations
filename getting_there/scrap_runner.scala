package getting_there

import com.sun.xml.internal.bind.v2.runtime.unmarshaller.XsiNilLoader
import fn_implementations.fn_implementations.{compose, curry}
import fn_implementations.completed_exercises
//import fpinscala.datastructures.List.{foldLeft, foldRight, product, sum, tail}
//import fpinscala.datastructures.{Cons,List,Nil}



object scrap_runner extends App {

//  def length[A](as: List[A]): Int = {
//    foldRight(as, 0)((_,n) => n + 1)
//  }
//
//  def append[A](xs: List[A], ys: List[A]): List[A] = foldRight(xs,ys)(Cons(_,_))

  def add1(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case h :: t => h + 1 :: add1(t)
  }

  def stringThatDouble(as: List[Double]): List[String] = as match {
    case Nil => Nil
    case h :: t => h.toString :: stringThatDouble(t)
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case x :: xs => f(x) :: map(xs)(f)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case x :: xs => if(f(x)) x :: xs else xs
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)



}
