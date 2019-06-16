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

  // 3.9
  def length[A](xs: List[A]): Int = {
    (0 /: xs)((x, _) => x+1)
  }

  def length2[A](xs: List[A]): Int = {
    (xs :\ 0)((_, x) => x + 1)
  }

  // 3.10
  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
    def fl(r: B, ys: List[A]): B = {
      ys match {
        case Nil => r
        case z :: zx => fl(f(r, z), zx)
      }
    }
    fl(z, xs)
  }

  // 3.11
  def sum(xs: List[Int]): Int = {
    (0 /: xs)(_ + _)
  }

  def product(xs: List[Int]): Int = {
    (1 /: xs)(_ * _)
  }

  // 3.12
  def reverse[T](xs: List[T]): List[T] = {
    (List[T]() /: xs)((ys, x) => x::ys)
  }

  // 3.13
  def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(xs), z)((a, b) => f(b, a))
  }

  // 3.14
  def append[T](xs: List[T], x: List[T]): List[T] = {
    foldRight(xs, x)((y, ys) => y::ys)
  }

  // 3.15
  def concat[T](xs: List[List[T]]): List[T] = {
    foldLeft(xs, List[T]())(append)
  }

  // 3.18
  def map[A, B](xs: List[A])(f: A => B): List[B] = {
    (xs :\ List[B]())((y, ys)=>f(y)::ys)
  }

  // 3.19
  def filter[A](xs: List[A])(f: A => Boolean): List[A] = {
    (xs :\ List[A]()){(y, ys) =>
      if (f(y)) y::ys else ys
    }
  }

  // 3.20
  def flatMap[A, B](xs: List[A])(f: A=>List[B]): List[B] = {
    (xs :\ List[B]())((y, ys) => f(y):::ys)
  }

  // 3.21
  def filterByFlatMap[A](xs: List[A])(f: A => Boolean): List[A] = {
    flatMap(xs)(y => if (f(y)) List(y) else Nil)
  }

  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = {
    (xs, ys) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (h1::t1, h2:: t2) => f(h1, h2)::zipWith(t1, t2)(f)
    }
  }

  // 3.24
  @annotation.tailrec
  def startWith[T](xs: List[T], prefix: List[T]): Boolean = {
    (xs, prefix) match {
      case (_, Nil) => true
      case (h::t, h1::t1) if h==h1 => startWith(t, t1)
      case _ => false
    }
  }
  @annotation.tailrec
  def hasSubSeq[T](sup: List[T], sub: List[T]): Boolean = {
    sup match {
      case Nil => sub == Nil
      case _ if startWith(sup, sub) => true
      case _::t  => hasSubSeq(t, sub)
    }
  }

  def main(args: Array[String]): Unit = {
    assert(cdr(List(1,2,3,4)) == List(2,3,4))
    assert(setHead(List(2), 1) == List(1))
    assert(drop(List(1,2,3,4), 2) == List(3,4))
    assert(dropWhile(List(1,2,3,4), (a: Int) => a %2==0) == List(1, 3))
    assert(dropWhile2(List(1,2,3,4), (a: Int) => a %2==0) == List(1, 3))
    assert(init(List(1,2,3,4)) == List(1,2,3))
    assert(length(List(1,2,3,4)) == 4)
    assert(length2(List(1,2,3,4)) == 4)
    assert(foldLeft(List(1,2,3,4), 1)(_ * _) == 24)
    assert(sum(List(1,2,3)) == 6)
    assert(product(List(1,2,3)) == 6)
    assert(reverse(List(1,2,3,4)) == List(4,3,2,1))
    assert(foldRight(List(1,2,3,4), 0)(_ + _) == 10)
    assert(append(List(1,2), List(3,4)) == List(1,2,3,4))
    assert(concat(List(List(1,2), List(3,4), List(5,6))) == List(1,2,3,4,5,6))
    assert(map(List(1,2,3,4))(_ + 1) == List(2,3,4,5)) // 3.18
    assert(filter(List(1,2,3,4))(_ % 2 == 0) == List(2,4)) // 3.19
    assert(flatMap(List(1,2))(i => List(i, i)) == List(1,1,2,2)) // 3.20
    assert(filterByFlatMap(List(1,2,3,4))(_ % 2 == 0) == List(2, 4)) // 3.21
    assert(zipWith(List(1,2,3), List(1,2,3))(_ + _) == List(2,4,6))
    assert(hasSubSeq(List(1,2,3,4), List(3,4)) == true) // 3.24
    assert(hasSubSeq(List(1,2,3,4), List(3,5)) == false) // 3.24
  }
}