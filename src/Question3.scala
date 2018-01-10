import java.io.{BufferedWriter, FileWriter}

import scala.collection.mutable

/**
  * Created by student on 20/12/2017.
  */
class Question3(filepath: String) extends QuestionInterface {
  protected val state: mutable.HashMap[DayAccountKey, AccountStatistics] = mutable.HashMap[DayAccountKey, AccountStatistics]()

  override def closure(): Unit = {
    //nop
  }

  override val headers: List[String] = List("transaction_day", "account_id", "max_amount", "average_amount", "aa_total", "cc_total", "ff_total")
  override protected val writer: BufferedWriter = new BufferedWriter(new FileWriter(filepath))

  def execOnChangeDay(day: Int): Unit = {
    Statistics.rolling(state, day, writer)
  }

  def exec(ts: Transaction): Unit = {
    val key = DayAccountKey(ts)
    if (state.contains(key)) {
      state(key).update(ts)
    } else {
      state.put(key, new AccountStatistics(Some(ts)))
    }
  }
}
