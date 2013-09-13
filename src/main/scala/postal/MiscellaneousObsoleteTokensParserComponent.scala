package postal

import scala.util.parsing.combinator.RegexParsers

trait MiscellaneousObsoleteTokensParserComponent {
  this: CoreRulesParserComponent =>

  trait MiscellaneousObsoleteTokensParser extends RegexParsers {
    this: CoreRulesParser =>
    /*
       4.1.  Miscellaneous Obsolete Tokens

       These syntactic elements are used elsewhere in the obsolete syntax or
       in the main syntax.  Bare CR, bare LF, and NUL are added to obs-qp,
       obs-body, and obs-unstruct.  US-ASCII control characters are added to
       obs-qp, obs-unstruct, obs-ctext, and obs-qtext.  The period character
       is added to obs-phrase.  The obs-phrase-list provides for a
       (potentially empty) comma-separated list of phrases that may include
       "null" elements.  That is, there could be two or more commas in such
       a list with nothing in between them, or commas at the beginning or
       end of the list.
     */

    /*
       obs-NO-WS-CTL   =   %d1-8 /            ; US-ASCII control
                           %d11 /             ;  characters that do not
                           %d12 /             ;  include the carriage
                           %d14-31 /          ;  return, line feed, and
                           %d127              ;  white space characters
     */
    protected[this] def obs_NO_WS_CTL: Parser[String] = "[\\u0001-\\u0008]".r | "\\u000B".r | "\\u000C".r | "[\\u000E-\\u001F]".r | "\\u007F".r

    // obs-ctext       =   obs-NO-WS-CTL
    protected[this] def obs_ctext: Parser[String] = obs_NO_WS_CTL

    // obs-qtext       =   obs-NO-WS-CTL
    protected[this] def obs_qtext: Parser[String] = obs_NO_WS_CTL

    // obs-qp          =   "\" (%d0 / obs-NO-WS-CTL / LF / CR)
    protected[this] def obs_qp: Parser[String] = "\\" ~ ("\\u0000".r | obs_NO_WS_CTL | LF | CR) ^^ {
      case "\\" ~ c => "\\" + c
    }
  }
}
