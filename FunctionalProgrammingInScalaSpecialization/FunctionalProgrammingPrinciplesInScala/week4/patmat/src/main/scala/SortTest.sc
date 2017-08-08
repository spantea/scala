def sort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, sort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}


def sortTest1 = sort(List(3, 2, 1, 5))
sortTest1