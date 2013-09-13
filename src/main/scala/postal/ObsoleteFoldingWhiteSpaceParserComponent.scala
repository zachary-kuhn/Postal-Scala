package postal

import scala.util.parsing.combinator.RegexParsers

trait ObsoleteFoldingWhiteSpaceParserComponent {
  this: CoreRulesParserComponent =>

  trait ObsoleteFoldingWhiteSpaceParser extends RegexParsers {
    this: CoreRulesParser =>
    /*
       4.2.  Obsolete Folding White Space

       In the obsolete syntax, any amount of folding white space MAY be
       inserted where the obs-FWS rule is allowed.  This creates the
       possibility of having two consecutive "folds" in a line, and
       therefore the possibility that a line which makes up a folded header
       field could be composed entirely of white space.
     */

    // obs-FWS         =       1*WSP *(CRLF 1*WSP)
    protected[this] def obs_FWS: Parser[String] = rep1(WSP) ~ rep(crlf_and_wsps) ^^ {
      case wsps ~ crlfs =>
        wsps.mkString + crlfs.mkString
    }

    private[this] def crlf_and_wsps: Parser[String] = CRLF ~ rep1(WSP) ^^ {
      case c ~ wsps => c + wsps.mkString
    }
  }
}
