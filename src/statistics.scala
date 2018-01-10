import java.io.BufferedWriter

import scala.collection.mutable

object Statistics {
  val rolling_days = 5

  def aggregate_stats(stats: Seq[AccountStatistics]): AccountStatistics = {
    val agg_stats = new AccountStatistics()
    agg_stats.account_max = stats.map(_.account_max).max
    agg_stats.account_sum = stats.map(_.account_sum).sum
    agg_stats.account_count = stats.map(_.account_count).sum
    agg_stats.aa_total = stats.map(_.aa_total).sum
    agg_stats.cc_total = stats.map(_.cc_total).sum
    agg_stats.ff_total = stats.map(_.ff_total).sum
    agg_stats
  }

  def rolling(calcs: mutable.HashMap[DayAccountKey, AccountStatistics], transaction_day: Int, writer: BufferedWriter): Unit = {
    //take aggregated per day and aggregate them based on past 5 days
    val keys = calcs.keySet.filter(_.day >= transaction_day - Statistics.rolling_days)

    val latest_account_ids = keys.map(_.id).toSet.toList.sorted
    val latest_days = keys.map(_.day).toSet.toList.sorted

    {
      //just a sanity check
      val left_set = keys.map(_.day).toSet
      val right_set = Range(List(transaction_day - Statistics.rolling_days, 1).max, transaction_day).toSet
      assert(left_set == right_set, (left_set, right_set))
    }

    for (acc_id <- latest_account_ids) {
      val cur_stats = keys.filter(_.id == acc_id).toList.map(key => calcs(key))

      val out_str = Statistics.aggregate_stats(cur_stats).render.to_str(Some(List(transaction_day, acc_id).mkString(",")))

      writer.write(out_str + "\n")
    }
  }
}

object AccCateg extends Enumeration {
  val AA = Value("AA")
  val CC = Value("CC")
  val FF = Value("FF")
}

case class AccRenderedStats(acc_max: Double, acc_avg: Double
                            , aa_total: Double, cc_total: Double, ff_total: Double
                           ) {
  def to_str(prefix: Option[String] = None): String = {
    List(prefix.getOrElse(""), acc_max, acc_avg, aa_total, cc_total, ff_total).mkString(",")
  }
}

class AccountStatistics(val ts: Option[Transaction] = None) {
  var account_max: Double = ts.map(_.transactionAmount).getOrElse(0)
  var account_sum: Double = ts.map(_.transactionAmount).getOrElse(0)
  var account_count: Long = if (ts.isDefined) 1 else 0
  var aa_total: Double = if (ts.map(_.category).orNull == AccCateg.AA.toString) ts.map(_.transactionAmount).getOrElse(0) else 0
  var cc_total: Double = if (ts.map(_.category).orNull == AccCateg.CC.toString) ts.map(_.transactionAmount).getOrElse(0) else 0
  var ff_total: Double = if (ts.map(_.category).orNull == AccCateg.FF.toString) ts.map(_.transactionAmount).getOrElse(0) else 0

  def update(ts: Transaction): Unit = {
    val cur_day = ts.transactionDay
    val cur_amount = ts.transactionAmount

    account_count += 1
    account_sum += cur_amount
    account_max = List(cur_amount, account_max).max

    try {
      AccCateg.withName(ts.category) match {
        case AccCateg.AA => aa_total += cur_amount
        case AccCateg.CC => cc_total += cur_amount
        case AccCateg.FF => ff_total += cur_amount
        case _ => //ignore other cases
      }
    } catch {
      case ee: NoSuchElementException => //ignore unknown cases
    }

  }

  def render: AccRenderedStats = {
    AccRenderedStats(acc_max = account_max, acc_avg = account_sum / account_count
      , aa_total = aa_total, cc_total = cc_total, ff_total = ff_total
    )
  }
}

object DayAccountKey {
  def apply(ts: Transaction): DayAccountKey = new DayAccountKey(ts.transactionDay, ts.accountId)
}

case class DayAccountKey(day: Int, id: String) {
  override def toString: String = {
    day + "," + id
  }
}
