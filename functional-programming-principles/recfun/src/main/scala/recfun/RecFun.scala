package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    def factorial(n: Int) = {
      def iter(n: Int, x: Int): Int =
        if (n == 0) x
        else iter(n - 1, x * n)
      iter(n, 1)
    }
    println(factorial(4))

    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == c) {
      1
    } else {
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def iter(chars: List[Char], tally: Int): Boolean = {
      if (tally < 0) {
        false
      } else if (chars.isEmpty) {
        tally == 0
      } else {
        val change = chars.head match {
          case '(' => 1
          case ')' => -1
          case _   => 0
        }
        iter(chars.tail, tally + change)
      }
    }
    iter(chars, 0)
  }

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def iter(money: Int, coins: List[Int]): Int =
      if (money < 0 || coins.isEmpty) {
        0
      } else if (money == 0) {
        1
      } else {
        iter(money, coins.tail) + iter(money - coins.head, coins)
      }
    iter(money, coins)
  }
}
