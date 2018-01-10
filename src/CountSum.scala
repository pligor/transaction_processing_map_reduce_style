/**
  * Created by student on 20/12/2017.
  */
case class CountSum(var sum: Double, var count: Long = 1) {
  def update(new_value: Double): Unit = {
    sum += new_value
    count += 1
  }

  def avg(): Double = sum / count
}
