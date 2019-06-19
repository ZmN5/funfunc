object OptionExam{
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = { 
    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = 
    a match {
      case Nil => Some(Nil)
      case h::t => h flatMap (hh => sequence(t) map (hh::_))
    }

  def main(args: Array[String]): Unit = {
    // println(map2(Some(1), Some(2))(_ + _))
    // println(map2(Some(1), None: Option[Int])(_ + _))
    // println(sequence(List(Some(1), Some(2))))
    println(sequence(List(Some(1), None)))
  }
}