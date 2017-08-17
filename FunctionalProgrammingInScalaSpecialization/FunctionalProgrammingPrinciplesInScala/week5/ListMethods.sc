import math.Ordering

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case x :: List() => List()
  case y :: ys => y :: init(ys)
}
val l1 = List(1, 2, 3)
init(l1)

def removeAt[T](n: Int, xs: List[T]): List[T] = n match {
  case 0 => xs.tail
  case m => xs.head :: removeAt(m - 1, xs.tail)
}

removeAt(1, List(1, 2, 3, 4))

def removeAt2[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)

removeAt2(1, List(1, 2, 3, 4))

def mergeSort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (List(), List()) => List()
      case (List(), _) => ys
      case (_, List()) => xs
      case (a :: as, b :: bs) => if (ord.lt(a, b)) a :: merge(as, ys) else b :: merge(xs, bs)
    }

    val (fst, scnd) = xs splitAt  n
    merge(mergeSort(fst), mergeSort(scnd))
  }
}

mergeSort(List(1, 3, 5 ,2 ,4 ,6, 0))

mergeSort(List("xanax", "oranges", "apples", "pears"))

def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => Nil
    case y :: ys => y * y :: squareList(ys)
  }

def squareList2(xs: List[Int]): List[Int] = xs map (x => x * x)

squareList(List(2, 4, 5))
squareList2(List(2, 4, 5))

//def pack[T](xs: List[T]): List[List[T]] = xs match {
//  case Nil => Nil
//  case x :: xs1 =>
//}
//
//pack(List(1, 1, 2, 3, 3, 3, 5, 5))


def isPrime(n: Int): Boolean = (2 until n).forall(x => n % x != 0)

isPrime(6)