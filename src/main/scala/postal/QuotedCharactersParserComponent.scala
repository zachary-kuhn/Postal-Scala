package postal

import scala.util.parsing.combinator.RegexParsers

trait QuotedCharactersParserComponent {
  this: CoreRulesParserComponent with MiscellaneousObsoleteTokensParserComponent =>

  trait QuotedCharactersParser extends RegexParsers {
    this: MiscellaneousObsoleteTokensParser with CoreRulesParser =>
    /*
       3.2.1.  Quoted characters

       Some characters are reserved for special interpretation, such as
       delimiting lexical tokens.  To permit use of these characters as
       uninterpreted data, a quoting mechanism is provided.
     */

    // quoted-pair     =   ("\" (VCHAR / WSP)) / obs-qp
    protected[this] def quoted_pair: Parser[String] = backslash_and_VCHAR_or_WSP ||| obs_qp

    private[this] def backslash_and_VCHAR_or_WSP: Parser[String] = "\\" ~ (VCHAR | WSP) ^^ {
      case "\\" ~ t => "\\" + t
    }
  }
}
