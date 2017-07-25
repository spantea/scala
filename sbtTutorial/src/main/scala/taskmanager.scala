import java.time.LocalDate

object TaskManager {
  def allTasksDueToday(tasks: List[Task]): List[Task] = {
    tasks.filter(t => LocalDate.now().equals(t.dueOn))
  }
}
