abstract class JSON
case class JSeq(seq: List[JSON]) extends JSON
case class JObject(map: Map[String, JSON]) extends JSON
case class JNumber(num: Int) extends JSON
case class JString(str: String) extends JSON
case class JBool(b: Boolean) extends JSON
case object JNull extends JSON

val data = JObject(Map(
  "firstName" -> JString("Luciana"),
  "lastName" -> JString("Tache"),
  "phoneNumbers" -> JSeq(List(
    JObject(Map(
      "phone1" -> JNumber(12345),
      "phone2" -> JNumber(12312)
    ))
  )),
  "address" -> JObject(Map(
    "street" -> JString("Cozla"),
    "city" -> JString("Bucharest")
  ))
))

def show(data: JSON): String = data match {
  case JSeq(elems) => "[" + (elems map show mkString) + "]"
  case JObject(bindings) =>
    val assocs = bindings map {
      case (key, value) => "\"" + key + "\": " + show(value)
    }
    "{" + (assocs mkString ", ") + "}"
  case JNumber(num) => num.toString
  case JString(str) => '\"' + str + '\"'
  case JBool(b) => b.toString
  case JNull => "null"
}

show(data)