import java.io.File

import scala.io.Source

//Define a case class Transaction which represents a transaction
case class Transaction(transactionId: String,
                       accountId: String,
                       transactionDay: Int,
                       category: String,
                       transactionAmount: Double)

/**
  * Created by Georgios Pligoropoulos
  * NOTE THAT NOTHING WILL WORK AS EXPECTED UNLESS THE DATA ARE SORTED (IN ASCENDING ORDER) BY TRANSACTION DAY
  */
object Main {
  def main(args: Array[String]): Unit = {
    val filepaths = List(1, 2, 3).map(ind => new File(".").getCanonicalPath + s"/question$ind.csv")

    val q1 = new Question1(filepaths.head)
    val q2 = new Question2(filepaths(1))
    val q3 = new Question3(filepaths(2))
    val qs = List(q1, q2, q3)

    val data_filename = new File(".").getCanonicalPath + "/data/transactions.txt"
    val reader = Source.fromFile(data_filename).bufferedReader()

    //we know that the first day has index 1
    var passed_day = 1
    try {
      qs.foreach(_.write_headers())

      //drop first line
      var cur_line = reader.readLine()
      var cur_day: Int = -1
      while ( {
        cur_line = reader.readLine()
        cur_line != null
      }) {
        val split = cur_line.split(',')
        val ts = Transaction(split(0), split(1), split(2).toInt, split(3), split(4).toDouble)
        cur_day = ts.transactionDay


        val onChangeDay = cur_day > passed_day
        if (onChangeDay) {
          passed_day = cur_day
          q1.execOnChangeDay(ts)
          q3.execOnChangeDay(cur_day)
        } else {
          q1.execOnSameDay(ts)
        }

        q2.exec(ts)
        q3.exec(ts)
      }

      q3.execOnChangeDay(cur_day + 1)

      qs.foreach(_.closure())
    } finally {
      reader.close()
      qs.foreach(_.finaly())
    }

    println("Files are ready:")
    for (filepath <- filepaths) {
      println(filepath)
    }
    println("Enjoy!")
  }

}

