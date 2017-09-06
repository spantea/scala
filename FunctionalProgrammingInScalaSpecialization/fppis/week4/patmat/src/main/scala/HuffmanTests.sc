import patmat.Huffman.Bit

def test(pair: (Char, Int), hs: List[(Char, Int)], xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
  case Nil => hs ::: pair :: Nil
  case (pair._1, n) :: ys => hs ::: (pair._1, n + 1) :: xs.tail
  case ys => test(pair, ys.head :: hs, ys.tail)
}

test(('A', 1), Nil, ('X', 1) :: ('B', 2) :: Nil)
test(('B', 1), Nil, ('A', 1) :: ('B', 2) :: Nil)
val t1 = test(('A', 1), Nil, Nil)
test(('A', 1), Nil, t1)

def times(chars: List[Char]): List[(Char, Int)] = {

  // search for (char, 1) in the xs list, if head doesn't match add to hs list and continue searching the tail
  def countChar(pair: (Char, Int), hs: List[(Char, Int)], xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
    case Nil => hs ::: pair :: Nil
    case (pair._1, n) :: ys => hs ::: (pair._1, n + 1) :: xs.tail
    case ys => countChar(pair, ys.head :: hs, ys.tail)
  }

  // loop over all the chars in the list
  def loop(chrs: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = {
    if (chrs.isEmpty) acc
    else loop(chrs.tail, countChar((chrs.head, 1), Nil, acc))
  }

  loop(chars, Nil)
}

times(List())
times(List('a', 'b', 'a'))
times(List('a', 'b', 'a', 'A', 'c', 'B'))



def test2(pair: (Char, Int), xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
  case Nil => pair :: Nil
  case y :: ys => if (y._1 == pair._1) (y._1, y._2 + 1) :: ys else y :: test2(pair, ys)
}

test2(('A', 1), ('X', 1) :: ('B', 2) :: Nil)
test2(('B', 1), ('A', 1) :: ('B', 2) :: Nil)
val t12 = test(('A', 1), Nil, Nil)
test2(('A', 1), t12)

def t1(xs: List[Int]): Boolean = xs match {
  case x::Nil => true
  case y::ys => false
  case Nil => false
}

t1(List(1))


def matchTest(bits: List[Bit]) = bits match {
  case 0::xs => println(234234)
}

matchTest(List(0))

val tl = 0 :: Nil ::: 1::Nil