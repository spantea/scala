import java.util.concurrent.{Callable, ExecutorService, Executors, Future}

def sumSegment(a: Array[Int], p: Int, s: Int, t: Int): Int = {
  var sum: Int = 0; var i = s
  while (i < t) {
    sum = sum + power(a(i), p)
    i = i + 1
  }
  sum
}

def power(x: Int, p: Double): Int = math.exp(p * math.log(Math.abs(x))).toInt

def pNorm(a: Array[Int], p: Int): Int = {
  power(sumSegment(a, p, 0, a.length), 1.0 / p)
}

def parallelPNorm(a: Array[Int], p: Int): Int = {
  val m = a.length / 2

  val (s1, s2) = parallel[Int, Int](sumSegment(a, p, 0, m), sumSegment(a, p, m, a.length))

  Math.pow(s1 + s2, 1.0 / p).toInt
}

def pNormRec(a: Array[Int], p: Int): Int = {
  power(segmentRec(a, p, 0, a.length, 1000), 1.0 / p)
}

def segmentRec(a: Array[Int], p: Int, s: Int, t: Int, threshold: Int): Int = {
  if (t - s < threshold) {
    sumSegment(a, p, s, t)
  } else {
    val m: Int =  s + (t - s) / 2
    val (s1, s2) = parallel(segmentRec(a, p, s, m, threshold), segmentRec(a, p, m + 1, t, threshold))
    s1 + s2
  }
}

def parallel[A, B](t1: => A, t2: => B): (A, B) = {

  val pool: ExecutorService = Executors.newFixedThreadPool(2)

  val future1: Future[A] = pool.submit(new Callable[A] {
    override def call(): A = t1
  })

  val future2: Future[B] = pool.submit(new Callable[B] {
    override def call(): B = t2
  })

  (future1.get(), future2.get())
}

val a: Array[Int] = Array(3, 4, 5)

sumSegment(a, 3, 0, a.length)

pNorm(a, a.length)

parallelPNorm(a, a.length)

pNormRec(a, a.length)

def task[A](c: => A): Task[A]

trait Task[A] {
  def join: A
}

def parallelTask[A, B](cA: => A, cB: => B): (A, B) = {
  val t1 = task(cA)
  val t2 = cB
  (t1.join, t2)
}