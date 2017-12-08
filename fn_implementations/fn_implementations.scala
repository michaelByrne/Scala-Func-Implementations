package fn_implementations

object fn_implementations {
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a: A) => { (b:B) => f(a,b) }
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a:A) => f(g(a))
  }
}
