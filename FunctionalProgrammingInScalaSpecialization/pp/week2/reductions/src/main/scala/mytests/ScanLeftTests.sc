def scanLeft[A](inp: Array[A], out: Array[A], init: A, f:(A, A) => A): Unit = {
  var i = 0
  var a = init
  out(0) = init

  while (i < inp.length) {
    a = f(a, inp(i))
    i = i + 1
    out(i) = a
  }
}

def reduceSeg[A](inp: Array[A], left: Int, right: Int, a0: A, f: (A, A) => A): A = {
  var idx = 0
  var temp = a0
  while (idx < right) {
    temp = f(temp, inp(idx))
    idx += 1
  }
  temp
}

def mapSeg[A](inp: Array[A], from: Int, until: Int, f: (Int) => A, out: Array[A]): Unit = {
  var idx = from
  while (idx < until) {
    out(idx) = f(idx)
    idx = idx + 1
  }
}

def scanLef[A](inp: Array[A], left: Int, right: Int, a0: A, f: (A, A) => A, out: Array[A]): Unit = {

  val fi = { (i: Int) => reduceSeg[A](inp, 0, i, a0, f) }
  mapSeg(inp, 0, inp.length, fi, out)
  val last = inp.length - 1
  out(last + 1) = f(out(last), inp(last))
}

val ar1 = Array(1, 4, 34, 100)
val ot1 = new Array[Int](5)

scanLef[Int](ar1, 0, ar1.length, 10, _ + _, ot1)

ot1

val ar = Array(1, 4, 34, 100)
val ot = new Array[Int](5)

scanLeft[Int](ar, ot, 10, _ + _)
ot

List(1, 4, 34, 100).scanLeft(10)(_ + _)