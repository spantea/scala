trait Subscriber {
  def handler(p: Publisher)
}
