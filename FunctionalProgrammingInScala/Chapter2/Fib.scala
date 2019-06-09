// 2.1
object Fib{
  def fib(n: Int): Int = {
    @annotation.tailrec
    def recur(first: Int, second: Int, m: Int): Int = {
      if (m == 0)
        first + second
      else
        recur(second, first + second, m - 1)
    }

    recur(0, 1, n)

  }

  def main(args: Array[String]): Unit = {
    if (args.length == 0){
      throw new Exception("error")
    }
    val res = fib(args(0).toInt)
    println(res)
  }
}