object ListImpl {
  // 3.2
  def cdr[T](xs: List[T]): List[T] = {
    xs match {
      case Nil => sys.error("tail of empty list")
      case y :: ys => ys
    }
  }
  
  // 3.3
  def setHead[T](xs: List[T], a: T): List[T] = {
    xs match {
      case Nil => sys.error("setHead on empty list")
      case y :: ys => a :: ys
    }
  }

  // 3.4
  def drop[A](xs: List[A], n: Int): List[A] = {
    (n, xs) match {
      case (0, _) => xs
      case (_, y::ys) => drop(ys, n-1)
      case (_, Nil) => sys.error("drop error")
    }
  }

  // 3.5
  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = {
    xs match {
      case Nil => xs
      case y :: ys if f(y) => dropWhile(ys, f)
      case y :: ys => y :: dropWhile(ys, f)
    }
  }

  def dropWhile2[A](xs: List[A], f: A => Boolean): List[A] = {
    @annotation.tailrec
    def go(leading: List[A], tailing: List[A]): List[A] = {
      tailing match {
        case Nil => leading ::: tailing
        case y :: ys if f(y) => go(leading, ys)
        case y :: ys => go(y :: leading, ys)
      }
    }
    go(Nil, xs).reverse
  }

  // 3.6
  def init[A](xs: List[A]): List[A] = {
    xs match {
      case Nil => sys.error("init error")
      case _ :: Nil => Nil
      case h::ys => h::init(ys)
    }
  }

  def main(args: Array[String]): Unit = {
    assert(cdr(List(1,2,3,4)) == List(2,3,4))
    assert(setHead(List(2), 1) == List(1))
    assert(drop(List(1,2,3,4), 2) == List(3,4))
    assert(dropWhile(List(1,2,3,4), (a: Int) => a %2==0) == List(1, 3))
    assert(dropWhile2(List(1,2,3,4), (a: Int) => a %2==0) == List(1, 3))
    println(init(List(1,2,3,4)))
  }

}