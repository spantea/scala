def sort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, sort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}


val sortTest1 = sort(List(3, 2, 1, 8, 5))
sortTest1

val l = 1 :: 2 :: 3 :: Nil
val l2 = List(1, 2, 3)

1::l

l.equals(l2)

l.head
l.tail

