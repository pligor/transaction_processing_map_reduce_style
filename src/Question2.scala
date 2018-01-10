import java.io.{BufferedWriter, FileWriter}

import scala.collection.mutable

/**
  * Created by student on 20/12/2017.
  */
class Question2(filepath: String) extends QuestionInterface {
  protected val state: mutable.HashMap[String, CountSum] = mutable.HashMap[String, CountSum]()

  override def closure(): Unit = {
    for (tpl <- state) {
      writer.write(tpl._1 + "," + tpl._2.avg() + "\n")
    }
  }

  override val headers: List[String] = List("account_category", "average_amount")
  override protected val writer: BufferedWriter = new BufferedWriter(new FileWriter(filepath))

  def exec(ts: Transaction): Unit = {
    if (state.contains(ts.category)) {
      state(ts.category).update(ts.transactionAmount)
    } else {
      state.put(ts.category, CountSum(sum = ts.transactionAmount))
    }
  }
}
