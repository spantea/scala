package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      def loop(c: Int, r: Int): Int =
        if (c - 1 < 0 || r == c) 1
        else loop(c - 1, r - 1) + loop(c, r - 1)

      loop(c, r)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(count: Int, chars: List[Char]): Boolean =
        if (chars.isEmpty) count == 0
        else if (count < 0) false
        else if (chars.head == '(') loop(count + 1, chars.tail)
        else if (chars.head == ')') loop(count - 1, chars.tail)
        else loop(count, chars.tail)

      loop(0,chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (coins.isEmpty && money > 0 || money < 0) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
