package mytests

import common._

class ScanLeftArray {

  val threshold = 10000

  sealed abstract class Tree[A]
  case class Leaf[A](a: A) extends Tree[A]
  case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  sealed abstract class TreeRes[A] {val res: A}
  case class LeafRes[A](from: Int, to: Int,
                     override val res: A) extends TreeRes[A]
  case class NodeRes[A](l: TreeRes[A],
                     override val res: A,
                     r: TreeRes[A]) extends TreeRes[A]

  def reduceSeg[A](inp: Array[A], left: Int, right: Int, a0: A, f: (A, A) => A): A = {
    var idx = 0
    var temp = a0
    while (idx < right) {
      temp = f(temp, inp(idx))
      idx += 1
    }
    temp
  }

  private def upsweep[A](inp: Array[A], from: Int, to: Int, f: (A, A) => A): TreeRes[A] = {
    if (to - from < threshold) {
      LeafRes(from, to, reduceSeg(inp, from + 1, to, inp(from), f))
    } else {
      val mid = from + (to - from) / 2
      val (tL, tR) = parallel(upsweep(inp, from, mid, f), upsweep(inp, mid, to, f))
      NodeRes(tL, f(tL.res, tR.res), tR)
    }
  }

  private def downsweep[A](inp: Array[A],
                           a0: A,
                           f: (A, A) => A,
                           t: TreeRes[A],
                           out: Array[A]): Unit = t match {
    case LeafRes(from, to, v) => scanLeftSeg(inp, from, to, v, f, out)
    case NodeRes(l, _, r) => {
      val (_, _) = parallel(
        downsweep(inp, a0, f, l, out),
        downsweep(inp, f(a0, l.res), f, r, out))
    }
  }

  private def scanLeftSeg[A](inp: Array[A], from: Int, to: Int, init: A, f:(A, A) => A, out: Array[A]): Unit = {
    var i = from
    var a = init
    out(0) = init

    while (i < to) {
      a = f(a, inp(i))
      i = i + 1
      out(i) = a
    }
  }

  def scanLeft[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]): Unit = {
    val tRes = upsweep(inp, 0, inp.length, f)
    downsweep(inp, a0, f, tRes, out)
    out(0) = a0
  }

  def main(args: Array[String]): Unit = {

  }
}
