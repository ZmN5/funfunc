sealed trait Tree[+T]
case class Leaf[T](value: T) extends Tree[T]
case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

object Tree {
  def size(t: Tree[T]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximum(t: Tree[T]): Int = {
    t match {
      case Leaf(n) => n
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def depth(t: Tree[T]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map(t: Tree[T])(f: T => B): Tree[B] = {
    t match {
      case Leaf(n) => Leaf(f(n))
      case Branch(l, r) => Branch(f(l), f(r))
    }
  }

  def fold(t: Tree[T])(f: T => B)(g: (B, B)=>B): T = {
    t match {
      case Leaf(a) => f(a)
      case Brach(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }
}