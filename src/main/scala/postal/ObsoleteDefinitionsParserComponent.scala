package postal

import scala.util.parsing.combinator.RegexParsers

trait ObsoleteDefinitionsParserComponent {
  this: AtomParserComponent with MiscellaneousTokensParserComponent with MiscellaneousObsoleteTokensParserComponent with QuotedCharactersParserComponent with CoreRulesParserComponent =>

  trait ObsoleteDomainParser extends RegexParsers {
    protected def obs_domain: Parser[String]

    protected def obs_dtext: Parser[String]
  }

  trait ObsoleteDefinitionsParser extends ObsoleteDomainParser {
    this: AtomParser with MiscellaneousTokensParser with MiscellaneousObsoleteTokensParser with QuotedCharactersParser with CoreRulesParser =>
    /*
       4.4.  Obsolete Addressing

       There are four primary differences in addressing.  First, mailbox
       addresses were allowed to have a route portion before the addr-spec
       when enclosed in "<" and ">".  The route is simply a comma-separated
       list of domain names, each preceded by "@", and the list terminated
       by a colon.  Second, CFWS were allowed between the period-separated
       elements of local-part and domain (i.e., dot-atom was not used).  In
       addition, local-part is allowed to contain quoted-string in addition
       to just atom.  Third, mailbox-list and address-list were allowed to
       have "null" members.  That is, there could be two or more commas in
       such a list with nothing in between them, or commas at the beginning
       or end of the list.  Finally, US-ASCII control characters and quoted-
       pairs were allowed in domain literals and are added here.
     */

    // obs-local-part  =       word *("." word)
    protected def obs_local_part: Parser[String] = word ~ rep(dot_and_word) ^^ {
      case w ~ ws => w + ws.mkString
    }

    private def dot_and_word: Parser[String] = "." ~ word ^^ {
      case "." ~ w => "." + w
    }

    // obs-domain      =       atom *("." atom)
    protected def obs_domain: Parser[String] = atom ~ rep(dot_and_atom) ^^ {
      case a ~ as => a + as.mkString
    }

    private def dot_and_atom: Parser[String] = "." ~ atom ^^ {
      case "." ~ a => "." + a
    }

    // obs-dtext       =   obs-NO-WS-CTL / quoted-pair
    protected def obs_dtext: Parser[String] = obs_NO_WS_CTL ||| quoted_pair
  }

}
