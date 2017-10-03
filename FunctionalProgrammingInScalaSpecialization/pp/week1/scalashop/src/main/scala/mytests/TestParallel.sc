import org.scalameter.{Warmer, withWarmer}

import scalashop.{HorizontalBoxBlur, Img, VerticalBoxBlur}
import scala.util.Random

val width = 1000
val height = 1000
val src = new Img(width, height)

for {
  i <- 0 until width
  j <- 0 until height
  v = Random.nextInt(255)
} yield src.update(i, j, v)

val dst1 = new Img(width, height)
val dst2 = new Img(width, height)

withWarmer(new Warmer.Default) measure HorizontalBoxBlur.blur(src, dst1, 0, height, 1)

withWarmer(new Warmer.Default) measure HorizontalBoxBlur.parBlur(src, dst2, 4, 1)

withWarmer(new Warmer.Default) measure VerticalBoxBlur.blur(src, dst1, 0, width, 1)

withWarmer(new Warmer.Default) measure VerticalBoxBlur.parBlur(src, dst2, 4, 1)