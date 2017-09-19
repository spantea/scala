package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[Int]
      m <- oneOf(const(empty), genHeap)
    } yield insert(k, m)
  )

  lazy val nonEmptyHeap: Gen[H] = for {
      k <- arbitrary[Int]
      m <- oneOf(const(empty), nonEmptyHeap, nonEmptyHeap, nonEmptyHeap)
    } yield insert(k, m)

  lazy val emptyHeap: Gen[H] = const(empty)
  lazy val smallInteger: Gen[Int] = Gen.choose(-1000, 1000)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("two values, one min") = forAll(emptyHeap, smallInteger, smallInteger) { (h: H, a: Int, b: Int) =>
    if (a < b) findMin(insert(b, insert(a, h))) == a else findMin(insert(a, insert(b, h))) == b
  }

  property("insert one, delete one") = forAll(emptyHeap, smallInteger) { (h: H, a: Int) =>
    isEmpty(deleteMin(insert(a, h)))
  }

  property("size of melding two heaps") = forAll { (h1: H, h2: H) =>
    val h1Size = heapSize(h1)
    val h2Size = heapSize(h2)
    heapSize(meld(h1, h2)) == h1Size + h2Size
  }

  property("find elem after melding") = forAll(emptyHeap, emptyHeap) { (h1: H, h2: H) =>
    val a = arbitrary[Int].sample.get
    val b = arbitrary[Int].sample.get

    if (a < b) findMin(deleteMin(meld(insert(a, h1), insert(b, h2)))) == b
    else findMin(deleteMin(meld(insert(a, h1), insert(b, h2)))) == a
  }

  property("sorted list after multiple deletes") = forAll { (h: H) =>
    isSorted(sortedList(h))
  }

  property("delete min") = forAll(emptyHeap) { (h: H) =>

    val n1 = Gen.choose(0, 100).sample.get
    val n2 = Gen.choose(101, 200).sample.get
    val n3 = Gen.choose(201, 300).sample.get
    val hp = insert(n2, insert(n1, insert(n3, h)))

    findMin(hp) != findMin(deleteMin(hp))
  }

  property("min of two heaps") = forAll(nonEmptyHeap, nonEmptyHeap) { (h1: H, h2: H) =>
    val h1Min = findMin(h1)
    val h2min = findMin(h2)
    val meldMin = findMin(meld(h1, h2))
    meldMin == h1Min || meldMin == h2min
  }

  def heapSize(h: H): Int = {
    def loop(size: Int, hp: H): Int = {
      if (isEmpty(hp)) size
      else loop(size + 1, deleteMin(hp))
    }

    loop(0, h)
  }

  def sortedList(xs: H): List[Int] = {
    if (isEmpty(xs)) List()
    else findMin(xs) :: sortedList(deleteMin(xs))
  }

  def isSorted(xs: List[Int]): Boolean = xs match {
    case Nil => true
    case x :: Nil => true
    case x::xs => if (x > xs.head) false else isSorted(xs)
  }
}
