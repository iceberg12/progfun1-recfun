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
    require(r >= 0, "Row must be at least 0")
    require(c <= r, "Column must be less then Row")

    if (c == 0 | c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def check_balance(chars: List[Char], noOpenBra: Int): Boolean = {
        if (noOpenBra == 0 & chars.isEmpty) true

        var x = noOpenBra
        if (chars.head == '(') x += 1
        else if (chars.head == ')') x += -1

        if (x < 0 | (x > 0 & chars.tail.isEmpty)) false
        else if (x == 0 & chars.tail.isEmpty) true
        else if (x >= 0 & !chars.tail.isEmpty) check_balance(chars.tail, x)
        else true
      }
      check_balance(chars, 0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      require(money > 0, "Invalid or zero money.")
      require(!coins.isEmpty, "Empty coin list.")
      require(coins.length == coins.distinct.length, "Coin list is not unique.")

      def change(x: Int, y: List[Int]): Int = {
        // find the list of coins the possibly used
        val z = y.filter(y1 => x >= y1).sorted.reverse  // impose coin ordering to make chains of coins unique

        if (z.length == 0) 0
        else if (z.length == 1)
          if (x % z.head == 0) 1
          else 0
        else //z.length >= 2
          if (x == z.head) change(x, z.tail) + 1
          else
            //observe that all conditions above are to check validity of this recursive formula
            change(x, z.tail) + change(x - z.head, z)
      }
      change(money, coins)

    }
  }
