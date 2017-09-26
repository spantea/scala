trait Publisher {

  private var subscribers: Set[Subscriber] = Set()

  def subscribe(s: Subscriber): Unit =
    subscribers += s

  def unsubscribe(s : Subscriber): Unit =
    subscribers -= s

  def publish(): Unit =
    subscribers.foreach(_.handler(this))
}
