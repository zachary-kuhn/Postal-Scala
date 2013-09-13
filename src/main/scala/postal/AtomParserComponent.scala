package postal

import scala.util.parsing.combinator.RegexParsers

trait AtomParserComponent {
  this: FoldingWhiteSpaceAndCommentsParserComponent with CoreRulesParserComponent =>

  trait DotAtomParser extends RegexParsers {
    protected def dot_atom: Parser[String]
  }

  trait AtomParser extends DotAtomParser {
    this: FoldingWhiteSpaceAndCommentsParser with CoreRulesParser =>
    /*
       3.2.3.  Atom

       Several productions in structured header field bodies are simply
       strings of certain basic characters.  Such productions are called
       atoms.

       Some of the structured header field bodies also allow the period
       character (".", ASCII value 46) within runs of atext.  An additional
       "dot-atom" token is defined for those purposes.
     */

    /*
       atext           =   ALPHA / DIGIT /    ; Printable US-ASCII
                           "!" / "#" /        ;  characters not including
                           "$" / "%" /        ;  specials.  Used for atoms.
                           "&" / "'" /
                           "*" / "+" /
                           "-" / "/" /
                           "=" / "?" /
                           "^" / "_" /
                           "`" / "{" /
                           "|" / "}" /
                           "~"
     */
    protected def atext: Parser[String] = ALPHA | DIGIT | "!" | "#" | "$" | "%" | "&" | "'" | "*" | "+" | "-" | "/" | "=" | "?" | "^" | "_" | "`" | "{" | "|" | "}" | "~"

    // atom            =       [CFWS] 1*atext [CFWS]
    protected def atom: Parser[String] = opt(CFWS) ~ rep1(atext) ~ opt(CFWS) ^^ {
      case Some(lcfws) ~ atexts ~ Some(rcfws) => lcfws + atexts.mkString + rcfws
      case None ~ atexts ~ Some(rcfws) => atexts.mkString + rcfws
      case Some(lcfws) ~ atexts ~ None => lcfws + atexts.mkString
      case None ~ atexts ~ None => atexts.mkString
    }

    // dot-atom-text   =       1*atext *("." 1*atext)
    protected def dot_atom_text: Parser[String] = rep1(atext) ~ rep(dot_and_atexts) ^^ {
      case atexts ~ dot_atexts =>
        atexts.mkString + dot_atexts.mkString
    }

    private def dot_and_atexts: Parser[String] = "." ~ rep1(atext) ^^ {
      case "." ~ atexts => "." + atexts.mkString
    }

    // dot-atom        =       [CFWS] dot-atom-text [CFWS]
    protected def dot_atom: Parser[String] = opt(CFWS) ~ dot_atom_text ~ opt(CFWS) ^^ {
      case Some(lcfws) ~ t ~ Some(rcfws) => lcfws + t + rcfws
      case None ~ t ~ Some(rcfws) => t + rcfws
      case Some(lcfws) ~ t ~ None => lcfws + t
      case None ~ t ~ None => t
    }
  }
}
