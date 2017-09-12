val level =
"""------
  |--ST--
  |--oo--
  |--oo--
  |------""".stripMargin


val l = ("a", 1) :: ("b", 2) :: ("c", 3) :: Nil

l.filter(_._1 == "a")

case class T(r: Int, c: Int)

val t1:T = T(1, 2)
val t2:T = T(1, 2)

t1 == t2

1 #:: 2 #:: Stream.empty

val stream: Stream[Int] = Stream.empty
val lInts: List[Int] = 1 :: 2 :: 3 :: 4 :: Nil

lInts.foreach(_ #:: stream)

stream.take(4).toList

def isPrime(n: Int): Boolean = n match {
  case 0 => false
  case 1 | 2 => true
  case n => (2 to Math.sqrt(Math.abs(n)).toInt).forall(x => n % x != 0)
}

isPrime(-111)

((1000 to 100000).toStream filter isPrime)(0)


def toMyInt(ch: Char): Int = ch.toInt

List('1', '2', '3') map toMyInt




