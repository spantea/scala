import scalashop.Img

//src(2, 1)
//
//val a = src(1 - 1, 2 - 1); val b = src(1, 2 - 1); val c = src(1 + 1, 2 - 1); val d = src(1 - 1, 2);
//val e = src(1, 2); val f = src(1 + 1, 2); val g = src(1 - 1, 2 + 1); val h = src(1, 2 + 1); val i = src(1 + 1, 2 + 1)
//
//val ravg: Int = (red(a) + red(b) + red(c) + red(d) + red(e) + red(f) + red(g) + red(h) + red(i)) / 9
//val gavg: Int = (green(a) + green(b) + green(c) + green(d) + green(e) + green(f) + green(g) + green(h) + green(i)) / 9
//val bavg: Int = (blue(a) + blue(b) + blue(c) + blue(d) + blue(e) + blue(f) + blue(g) + blue(h) + blue(i)) / 9
//val aavg: Int = (alpha(a) + alpha(b) + alpha(c) + alpha(d) + alpha(e) + alpha(f) + alpha(g) + alpha(h) + alpha(i)) / 9
//
//rgba(ravg, gavg, bavg, aavg)

type RGBA = Int

class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
  def this(w: Int, h: Int) = this(w, h, new Array(w * h))
  def apply(x: Int, y: Int): RGBA = data(y * width + x)
  def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
}

/** Returns the red component. */
def red(c: RGBA): Int = (0xff000000 & c) >>> 24

/** Returns the green component. */
def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

/** Returns the blue component. */
def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

/** Returns the alpha component. */
def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

/** Used to create an RGBA value from separate components. */
def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
  (r << 24) | (g << 16) | (b << 8) | (a << 0)
}

val src = new Img(3, 4)
src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16

val radius = 1
val x = 1; val y = 2
val xmin = x - radius
val ymin = y - radius

val channel_vals = for {
  i <- 0 until 2 * radius + 1
  j <- 0 until 2 * radius + 1
  xi = xmin + i
  yj = ymin + j
  if (xi >= 0 && xi < src.width && yj >= 0 && yj < src.height)
} yield (red(src(xi, yj)), green(src(xi, yj)), blue(src(xi, yj)), alpha(src(xi, yj)))

channel_vals.foldLeft((0, 0, 0, 0)) {
  case ((accA, accB, accC, accD), (a: Int, b: Int, c: Int, d: Int)) => (accA + a, accB + b, accC + c, accD + d)
}