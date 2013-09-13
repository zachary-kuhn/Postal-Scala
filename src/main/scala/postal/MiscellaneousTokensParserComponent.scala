package postal

import scala.util.parsing.combinator.RegexParsers

trait MiscellaneousTokensParserComponent {
  this: AtomParserComponent with QuotedStringsParserComponent =>

  trait MiscellaneousTokensParser extends RegexParsers {
    this: AtomParser with QuotedStringsParser =>
    /*
       3.2.5.  Miscellaneous Tokens

       Three additional tokens are defined: word and phrase for combinations
       of atoms and/or quoted-strings, and unstructured for use in
       unstructured header fields and in some places within structured
       header fields.
     */

    // word            =       atom / quoted-string
    protected[this] def word: Parser[String] = atom ||| quoted_string
  }
}
