import java.io.BufferedWriter

/**
  * Created by student on 20/12/2017.
  */
trait QuestionInterface {
  def closure()
  def finaly(): Unit = {
    writer.close()
  }

  val headers: List[String]

  protected val writer: BufferedWriter

  //def closure()

  def write_headers(): Unit = {
    writer.write(headers.mkString(",") + "\n")
  }
}
