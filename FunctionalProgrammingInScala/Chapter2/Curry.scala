object Curry{
  // 2.3
  def curry[A, B ,C](f: (A, B) => C): A => (B => C) =
    (a: A) => f(a: A, _)

  // 2.3
  def curry2[A, B ,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)
  
  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = 
    (a: A, b: B) => f(a)(b)

  // 2.5
  def compose[A, B, C](f: B=>C, g: A=>B): A=> C = 
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    assert(curry((a: Int, b: Int) => (a * b))(2)(3) == 6)
    assert(curry2((a: Int, b: Int) => (a * b))(2)(3) == 6)
    assert(uncurry((a: Int) => (b: Int) => a * b)(2, 3) == 6)
    assert(compose((a: Int)=> a * 2, (b: Int) => b * 3)(1) == 6)
  }
}