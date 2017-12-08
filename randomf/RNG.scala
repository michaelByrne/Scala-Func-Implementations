package randomf

import scala.math.abs
import scala.collection.immutable

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val(num, r) = rng.nextInt
    if (num < 1) return (-num + 1, r)
    (num, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val(num, r) = rng.nextInt
    (num.toDouble / Int.MaxValue.toDouble, r)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (i, r) = nextInt
    if (count > 0) return (i :: ints(count - 1)(r)._1,r)
    (List(),r)
  }

  type Rand[+A] = RNG => (A, RNG)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

}