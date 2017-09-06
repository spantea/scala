case class Book(title: String, authors: List[String])

val books: Set[Book] = Set(
  Book(title = "Structure and Interpretation of Computer Programs",
    authors = List("Abelson, Harald", "Sussman, Gerald J.")),
  Book(title = "Introduction to Functional Programming",
    authors = List("Bird, Richard", "Wadler, Phil")),
  Book(title = "Effective Java",
    authors = List("Bloch, Joshua")),
  Book(title = "Java Puzzlers",
    authors = List("Bloch, Joshua", "Gafter, Neal")),
  Book(title = "Programming in Scala",
    authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
)

for (b <- books; a <- b.authors if a startsWith "Bird") yield b.title


for (b <- books if b.title.indexOf("Program") >= 0) yield b.title

for {
  b1 <- books
  b2 <- books
  if b1 != b2
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield a1


(1 to 100).flatMap(i => (1 to i).withFilter(j => (i + j) % 2 == 0).map(j => (i, j)))


books.flatMap(b => for (a <- b.authors; if a.startsWith("Bird")) yield b.title)

books.flatMap(b => for (a <- b.authors withFilter(a => a.startsWith("Bird"))) yield b.title)

//books.flatMap(b => b.authors withFilter(a => a.startsWith("Bird")) map(y => y.title))


(1 #:: 2 #:: 3 #:: Stream.empty).take(2) toList

def expr = {
  val x = {print("x"); 1}
  lazy val y = {print("y"); 2}
  def z = {print("z"); 3}
  z + y + x + z + y + x
}

expr