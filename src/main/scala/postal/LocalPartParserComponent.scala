package postal

import scala.util.parsing.combinator.RegexParsers

trait LocalPartParserComponent {
  this: AtomParserComponent with QuotedStringsParserComponent with ObsoleteDefinitionsParserComponent with LengthComponent =>

  trait LocalPartParser extends RegexParsers {
    protected def local_part: Parser[String]
  }

  trait BaseLocalPartParser extends LocalPartParser {
    this: AtomParser with QuotedStringsParser with ObsoleteDefinitionsParser =>

    // local-part      =       dot-atom / quoted-string / obs-local-part
    protected def local_part: Parser[String] = dot_atom ||| quoted_string ||| obs_local_part
  }

  trait LocalPartLengthRestriction extends LocalPartParser {
    /*
       RFC 5321

       4.5.3.1.1.  Local-part

       The maximum total length of a user name or other local-part is 64
       octets.
     */
    protected abstract override def local_part: Parser[String] = super.local_part filter Length.lessThanOrEqualTo64
  }

}
