

val text = List("f1", "f2", "f3", "a1", "a2", "a3", "b1", "b2", "b3")

val nums = List("1", "2", "3")

nums.flatMap(n => List((n, text.filter(t => t.contains(n))))).toMap

