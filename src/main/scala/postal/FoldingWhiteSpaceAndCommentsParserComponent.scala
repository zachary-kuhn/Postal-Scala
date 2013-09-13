package postal

import scala.util.parsing.combinator.RegexParsers

trait FoldingWhiteSpaceAndCommentsParserComponent {
  this: QuotedCharactersParserComponent with ObsoleteFoldingWhiteSpaceParserComponent with MiscellaneousObsoleteTokensParserComponent with CoreRulesParserComponent =>

  trait FoldingWhiteSpaceAndCommentsParser extends RegexParsers {
    this: QuotedCharactersParser with ObsoleteFoldingWhiteSpaceParser with MiscellaneousObsoleteTokensParser with CoreRulesParser =>
    /*
       3.2.2.  Folding White Space and Comments

       White space characters, including white space used in folding
       (described in section 2.2.3), may appear between many elements in
       header field bodies.  Also, strings of characters that are treated as
       comments may be included in structured field bodies as characters
       enclosed in parentheses.  The following defines the folding white
       space (FWS) and comment constructs.

       Strings of characters enclosed in parentheses are considered comments
       so long as they do not appear within a "quoted-string", as defined in
       section 3.2.4.  Comments may nest.

       There are several places in this specification where comments and FWS
       may be freely inserted.  To accommodate that syntax, an additional
       token for "CFWS" is defined for places where comments and/or FWS can
       occur.  However, where CFWS occurs in this specification, it MUST NOT
       be inserted in such a way that any line of a folded header field is
       made up entirely of WSP characters and nothing else.
     */

    /*
       FWS             =   ([*WSP CRLF] 1*WSP) /  obs-FWS
                                              ; Folding white space
     */
    protected[this] def FWS: Parser[String] = new_FWS ||| obs_FWS

    private[this] def new_FWS: Parser[String] = opt(rep(WSP) ~ CRLF) ~ rep1(WSP) ^^ {
      case Some(wsps ~ crlf) ~ ewsps =>
        wsps.mkString + crlf + ewsps.mkString
      case None ~ ewsps =>
        ewsps.mkString
    }

    /*
       ctext           =   %d33-39 /          ; Printable US-ASCII
                           %d42-91 /          ;  characters not including
                           %d93-126 /         ;  "(", ")", or "\"
                           obs-ctext
     */
    protected[this] def ctext: Parser[String] = "[\\u0021-\\u0027]".r | "[\\u002A-\\u005B]".r | "[\\u005D-\\u007E]".r | obs_ctext

    // ccontent        =       ctext / quoted-pair / comment
    protected[this] def ccontent: Parser[String] = ctext ||| quoted_pair ||| comment

    // comment         =       "(" *([FWS] ccontent) [FWS] ")"
    protected[this] def comment: Parser[String] = "(" ~ rep(opt_FWS_and_ccontent) ~ opt(FWS) ~ ")" ^^ {
      case lp ~ ccontents ~ Some(fws) ~ rp =>
        lp + ccontents.mkString + fws + rp
      case lp ~ ccontents ~ None ~ rp =>
        lp + ccontents.mkString + rp
    }

    private[this] def opt_FWS_and_ccontent: Parser[String] = opt(FWS) ~ ccontent ^^ {
      case Some(f) ~ c =>
        f + c
      case None ~ c =>
        c
    }

    // CFWS            =   (1*([FWS] comment) [FWS]) / FWS
    protected[this] def CFWS: Parser[String] = repeating_opt_FWS_and_comment_and_opt_FWS ||| FWS

    private[this] def repeating_opt_FWS_and_comment_and_opt_FWS: Parser[String] = rep1(opt_FWS_and_comment) ~ opt(FWS) ^^ {
      case cs ~ Some(f) => cs.mkString + f
      case cs ~ None => cs.mkString
    }

    private[this] def opt_FWS_and_comment: Parser[String] = opt(FWS) ~ comment ^^ {
      case Some(fws) ~ comment =>
        fws + comment
      case None ~ comment =>
        comment
    }
  }
}
