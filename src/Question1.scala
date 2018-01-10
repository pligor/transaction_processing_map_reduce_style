import java.io.{BufferedWriter, FileWriter}

class Question1(filepath: String) extends QuestionInterface {
  //we know that the first day has index 1
  protected val state = DailyTotal(1)

  override val headers: List[String] = List("transaction_day", "total_amount")

  override protected val writer = new BufferedWriter(new FileWriter(filepath))

  def execOnChangeDay(ts: Transaction): Unit = {
    writer.write(state.toString() + "\n") //write the before resetting
    state.reset(ts.transactionDay, ts.transactionAmount)
  }

  def execOnSameDay(ts: Transaction): Unit = {
    state.add_amount(ts.transactionAmount)
  }

  override def closure() {
    writer.write(state.toString() + "\n")
  }
}

case class DailyTotal(var key_day: Int, var total: Double = 0) {
  def reset(day: Int, total: Double): Unit = {
    this.key_day = day
    this.total = total
  }

  override def toString: String = {
    super.toString
    this.key_day + "," + this.total
  }

  def add_amount(new_amount: Double): Unit = {
    this.total += new_amount
  }
}