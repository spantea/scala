//def permutations(x: String): Seq[String] = {
//
//}

val x = "abc"

def permutations(prefix: String, x: String): Unit = {
  if (x.length() == 0) println(prefix)
  else {
    var idx = 0
    while (idx < x.length) {
      permutations(prefix + x(idx), x.substring(0, idx) + x.substring(idx + 1, x.length))
      idx += 1
    }
  }
}

permutations("", "abc")
