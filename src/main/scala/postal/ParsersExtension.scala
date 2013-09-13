package postal

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import scala.util.parsing.combinator.Parsers

trait ParsersExtension extends Parsers {
  def repRange[T](min: Int, max: Int, p: => Parser[T]): Parser[List[T]] =
    if (min == 0) success(Nil) else Parser { in =>
      val elems = new ListBuffer[T]
      val p0 = p    // avoid repeatedly re-evaluating by-name parser

      @tailrec def applyp(in0: Input): ParseResult[List[T]] =
        if (elems.length == max) Success(elems.toList, in0)
        else p0(in0) match {
          case Success(x, rest) => elems += x ; applyp(rest)
          case ns: NoSuccess if elems.length >= min => Success(elems.toList, in0)
          case ns: NoSuccess    => ns
        }

      applyp(in)
    }
}
