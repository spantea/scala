
import barneshut.Body

case class Planet(mass: Float, size: Float, name: String)

val system = List(Planet(123f, 18f, "Earth"))

system.foldLeft(0f)((acc: Float, p: Planet) => acc + p.mass * p.size) / 123f

system.map(_.mass).sum

