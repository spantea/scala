import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import quickcheck.{BinomialHeap, Heap}

lazy val genMap: Gen[Map[Int,Int]] = oneOf(
  const(Map.empty[Int,Int]),
  for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)
)

genMap.sample

//lazy val genHeap: Gen[Heap] = oneOf(
//  const(BinomialHeap.e),
//  for {
//    k <- arbitrary[Int]
//    m <- oneOf(const(Map.empty[Int,Int]), genMap)
//  } yield m.updated(k, v)
//)



def isSorted(xs: List[Int]): Boolean = xs match {
  case Nil => true
  case x :: Nil => true
  case x::xs => if (x > xs.head) false else isSorted(xs)
}

isSorted(List(1, 3, 2))
isSorted(List(1, 2, 5, 3))
isSorted(List(1))
isSorted(List(5, -1, 1, 2, 3))
isSorted(List())