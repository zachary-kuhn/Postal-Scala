package postal

import scala.util.parsing.combinator.RegexParsers

trait QuotedStringsParserComponent {
  this: FoldingWhiteSpaceAndCommentsParserComponent with QuotedCharactersParserComponent with MiscellaneousObsoleteTokensParserComponent with CoreRulesParserComponent =>

  trait QuotedStringsParser extends RegexParsers {
    this: FoldingWhiteSpaceAndCommentsParser with QuotedCharactersParser with MiscellaneousObsoleteTokensParser with CoreRulesParser =>
    /*
       3.2.4.  Quoted Strings

       Strings of characters that include characters other than those
       allowed in atoms can be represented in a quoted string format, where
       the characters are surrounded by quote (DQUOTE, ASCII value 34)
       characters.
     */

    /*
       qtext           =   %d33 /             ; Printable US-ASCII
                           %d35-91 /          ;  characters not including
                           %d93-126 /         ;  "\" or the quote character
                           obs-qtext
     */
    protected[this] def qtext: Parser[String] = "\\u0021".r | "[\\u0023-\\u005B]".r | "[\\u005D-\\u007E]".r | obs_qtext

    // qcontent        =       qtext / quoted-pair
    protected[this] def qcontent: Parser[String] = qtext ||| quoted_pair

    /*
       quoted-string   =       [CFWS]
                               DQUOTE *([FWS] qcontent) [FWS] DQUOTE
                               [CFWS]
     */
    protected[this] def quoted_string: Parser[String] = opt(CFWS) ~ DQUOTE ~ rep(opt_FWS_and_qcontent) ~ opt(FWS) ~ DQUOTE ^^ {
      case Some(c) ~ lq ~ qs ~ Some(f) ~ rq =>
        c + lq + qs.mkString + f + rq
      case None ~ lq ~ qs ~ Some(f) ~ rq =>
        lq + qs.mkString + f + rq
      case None ~ lq ~ qs ~ None ~ rq =>
        lq + qs.mkString + rq
    }

    private[this] def opt_FWS_and_qcontent: Parser[String] = opt(FWS) ~ qcontent ^^ {
      case Some(f) ~ q => f + q
      case None ~ q => q
    }
  }
}
