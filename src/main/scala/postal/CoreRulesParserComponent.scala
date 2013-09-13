package postal

import scala.util.parsing.combinator.RegexParsers

trait CoreRulesParserComponent {
  trait CoreRulesParser extends RegexParsers {
    /*
       RFC 5234

       B.1.  Core Rules

       Certain basic rules are in uppercase, such as SP, HTAB, CRLF, DIGIT,
       ALPHA, etc.
     */

    // ALPHA          =  %x41-5A / %x61-7A   ; A-Z / a-z
    protected[this] def ALPHA: Parser[String] = "[\\u0041-\\u005A]".r | "[\\u0061-\\u007A]".r

    /*
       CR             =  %x0D
                              ; carriage return
     */
    protected[this] def CR: Parser[String] = "\\u000D".r

    /*
       CRLF           =  CR LF
                              ; Internet standard newline
     */
    protected[this] def CRLF: Parser[String] = CR ~ LF ^^ { case c ~ l => c + l }

    /*
       DIGIT          =  %x30-39
                              ; 0-9
     */
    protected[this] def DIGIT: Parser[String] = "[\\u0030-\\u0039]".r

    /*
       DQUOTE         =  %x22
                              ; " (Double Quote)
     */
    protected[this] def DQUOTE: Parser[String] = "\\u0022".r

    // HEXDIG         =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
    protected def HEXDIG: Parser[String] = DIGIT | "A" | "B" | "C" | "D" | "E" | "F"

    /*
       HTAB           =  %x09
                              ; horizontal tab
     */
    protected[this] def HTAB: Parser[String] = "\\u0009".r

    /*
       LF             =  %x0A
                              ; linefeed
     */
    protected[this] def LF: Parser[String] = "\\u000A".r

    // SP             =  %x20
    protected[this] def SP: Parser[String] = "\\u0020".r

    /*
       VCHAR          =  %x21-7E
                              ; visible (printing) characters
     */
    protected[this] def VCHAR: Parser[String] = "[\\u0021-\\u007E]".r

    /*
       WSP            =  SP / HTAB
                              ; white space
     */
    protected[this] def WSP: Parser[String] = SP | HTAB
  }

}
