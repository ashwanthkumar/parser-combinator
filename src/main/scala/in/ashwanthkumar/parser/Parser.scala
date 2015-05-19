package in.ashwanthkumar.parser

trait ParsedResult[+T] {

  def ^^[U](f: (T) => U): ParsedResult[U] = map(f)
  def map[U](f: (T) => U): ParsedResult[U]
  def get: T
}

abstract class Parser[T] { me =>
  def apply(in: String): ParsedResult[T]

  case class Success[+Input](result: Input, more: String) extends ParsedResult[Input] {
    override def map[U](f: (Input) => U): ParsedResult[U] = Success(f.apply(result), more)

    override def get: Input = result
  }

  case class Failure(msg: String, next: String) extends ParsedResult[Nothing] {
    override def map[U](f: Nothing => U) = this

    override def get: Nothing = sys.error("Failure has no value to get")
  }

  private def Parser[K](f: String => ParsedResult[K]): Parser[K] = new Parser[K] {
    def apply(in: String) = f(in)
  }

  def parse(input: String) = apply(input)

  def map[K](fn: (T) => K) = Parser { cs => this(cs) }

}

object Parser {

  def char(c: Char): Parser[Char] = new Parser[Char] {
    def apply(in: String) = in.toList match {
      case firstChar :: rest if firstChar == c => Success(firstChar, rest.mkString)
      case firstChar :: rest => Failure(s"$c expected but $firstChar found", in) // we back track upon failure
    }
  }

  def literal(str: String): Parser[String] = new Parser[String] {
    override def apply(in: String): ParsedResult[String] = in match {
      case x if in.startsWith(str) => Success(str, in.drop(str.length))
      case x => Failure(s"$str expected but $x found", in)
    }
  }

}
