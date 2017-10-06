package mytests

import org.scalameter._

import scalashop.{HorizontalBoxBlur, Img, VerticalBoxBlur}
import scala.util.Random

object TestPar extends App{

  val width = 320
  val height = 320
  val src = new Img(width, height)

  for {
    i <- 0 until width
    j <- 0 until height
    v = Random.nextInt(255)
  } yield src.update(i, j, v)

  val dst1 = new Img(width, height)
  val dst2 = new Img(width, height)
  val dst3 = new Img(width, height)
  val dst4 = new Img(width, height)

  println(withWarmer(new Warmer.Default) measure HorizontalBoxBlur.blur(src, dst1, 0, height, 1))

  println(withWarmer(new Warmer.Default) measure HorizontalBoxBlur.parBlur(src, dst2, 4, 1))

  println(withWarmer(new Warmer.Default) measure VerticalBoxBlur.blur(src, dst3, 0, width, 1))

  println(withWarmer(new Warmer.Default) measure VerticalBoxBlur.parBlur(src, dst4, 32, 1))
}
