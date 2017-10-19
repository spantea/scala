import common._

val numElems = 20

def combine: Array[Int] = {
  val array = new Array[Int](numElems)
  val step = math.max(1, numElems / 4)
  val starts = (0 until numElems by step) :+ numElems
  val chunks = starts.zip(starts.tail)

  val tasks = for ((from, to) <- chunks) yield task {
//    copyTo(array, from, to)
  }

  tasks.foreach(_.join())
  array
}

