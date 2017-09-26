import sun.misc.Signal

object TestConsolidator extends App {
  val a = new BankAccount
  val b = new BankAccount

  val c = new Consolidator(List(a, b))
  c.totalBalance()
  a.deposit(100)
  a.currentBalance
  b.deposit(50)
  b.currentBalance
  c.totalBalance()
  println(c.totalBalance())

  Map(1 -> 'A', 2 -> 'B') updated (1, 'X')
  Array(1, 2, 3, 4) update (0, 0)
}
