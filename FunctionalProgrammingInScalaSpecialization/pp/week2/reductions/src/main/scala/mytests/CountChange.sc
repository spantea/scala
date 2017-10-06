def countChange(money: Int, coins: List[Int]): Int = {
  if(money == 0)  1
  else if (money < 0 || coins.isEmpty) 0
  else countChange(money - coins.head, coins) + countChange(money, coins.tail)
}

countChange(4, List(1, 2))

def balance(chars: Array[Char]): Boolean = {
  var i = 0
  var nb = 0
  while (i < chars.length) {
    if (chars(i) == '(') nb += 1
    else if (chars(i) == ')') {
      nb -= 1
      if (nb < 0) {
        return false
      }
    }
    i += 1
  }
  if (nb % 2 == 0) true
  else false
}

def balance(chars: List[Char]): Boolean = {
  def loop(acc: Int, chars: List[Char]): Boolean = {
    if (chars.isEmpty) acc % 2 == 0
    else if (acc < 0) false
    else if (chars.head == '(') loop(acc + 1, chars.tail)
    else if (chars.head == ')') loop(acc - 1, chars.tail)
    else loop(acc, chars.tail)
  }

  loop(0, chars)
}