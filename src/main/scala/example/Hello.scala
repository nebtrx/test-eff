package example

object bla extends App{
  trait Ohhh[A]{
  }
  trait Ahhh[A]{

  }

  implicit val blaString: Ohhh[String] = new Ohhh[String] {}

  implicit val bla1Int: Ohhh[Int] = new Ohhh[Int] {}

  implicit val blaInt: Ahhh[Int] = new Ahhh[Int] {}

  implicit val bla1String: Ahhh[String] = new Ahhh[String] {}

  def conceptProof[T:Ohhh:Ahhh](p: T): Int = {
    p match {
      case _: String => println("TE cogi cabron SRING")
      case _: Int => println("TE fuiste cabrón INT")
    }
    7
  }

  conceptProof("haaaa")
  conceptProof(7)

}


object Hello extends App {

  import cats._, data._
  import org.atnos.eff._
  import org.atnos.eff.all._
  import org.atnos.eff.syntax.all._

  type ReaderInt[A] = Reader[Int, A]
  type WriterString[A] = Writer[String, A]

  type Stack = Fx.fx3[WriterString, ReaderInt, Eval]

  // useful type aliases showing that the ReaderInt and the WriterString effects are "members" of E
  // note that R could have more effects
  type _readerInt[E]    = ReaderInt |= E
  type _writerString[E] = WriterString |= E


  def program[E :_readerInt :_writerString :_eval]: Eff[E, String] = for {
    // get the configuration
    n <- ask[E, Int]

    // log the current configuration value
    _ <- tell(":::::: the required power is "+n)

    // compute the nth power of 2
    a <- delay(math.pow(2, n.toDouble).toInt)

    // log the result
    _ <- tell(":::::: The result is " + a)
  } yield a.toString

  // run the action with all the interpreters
  // each interpreter running one effect
  val result = program[Stack].runReader(6).runWriter.runEval.run
  println(s":::: RESULT $result")
}

trait Greeting {
  lazy val greeting: String = "hello"
}
