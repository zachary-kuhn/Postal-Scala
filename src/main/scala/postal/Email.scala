package postal

import scalaz._
import Scalaz._

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.RegexParsers
import scala.util.Try
import java.net.{URL, InetAddress}
import javax.naming.directory.InitialDirContext
import java.util.Hashtable
import javax.naming.Context
import com.sun.jndi.dns.DnsContext

sealed trait Email
case class ValidEmail(localAddress: String, domain: String) extends Email
case class InvalidEmail() extends Email

object Email extends EmailParserComponent
  with LengthComponent
  with LocalPartParserComponent
  with DomainParserComponent
  with AddressLiteralsParserComponent
  with AtomParserComponent
  with CoreRulesParserComponent
  with NumberComponent
  with FoldingWhiteSpaceAndCommentsParserComponent
  with MiscellaneousObsoleteTokensParserComponent
  with ObsoleteFoldingWhiteSpaceParserComponent
  with MiscellaneousTokensParserComponent
  with ObsoleteDefinitionsParserComponent
  with QuotedCharactersParserComponent
  with QuotedStringsParserComponent {
  import EmailParser.{ Failure, Success }
  /*def apply(email: String): Validation[String, Email] = {
    success(ValidEmail(email))
  }*/

  def isValid(email: String): Boolean = {
    EmailParser.parse(email) match {
      case Success(_,_) => true
      case _ => false
    }
  }
}
trait EmailParserComponent {
  this: LengthComponent
    with LocalPartParserComponent
    with DomainParserComponent
    with AddressLiteralsParserComponent
    with AtomParserComponent
    with CoreRulesParserComponent
    with FoldingWhiteSpaceAndCommentsParserComponent
    with MiscellaneousObsoleteTokensParserComponent
    with MiscellaneousTokensParserComponent
    with ObsoleteDefinitionsParserComponent
    with ObsoleteFoldingWhiteSpaceParserComponent
    with QuotedCharactersParserComponent
    with QuotedStringsParserComponent =>

  object EmailParser extends RegexParsers
    with BaseLocalPartParser
    with LocalPartLengthRestriction
    with BaseDomainParser
    with AddressLiteralsParser
    with AddressLiteralRestriction
    with DomainLengthRestriction
    with AtomParser
    with CoreRulesParser
    with FoldingWhiteSpaceAndCommentsParser
    with MiscellaneousObsoleteTokensParser
    with MiscellaneousTokensParser
    with ObsoleteDefinitionsParser
    with ObsoleteFoldingWhiteSpaceParser
    with QuotedCharactersParser
    with QuotedStringsParser {
    override val skipWhitespace = false

    def parse(email: String): ParseResult[Email] = parseAll(addr_spec, email)

    def main(args: Array[String]) {
    }

    /*
       3.4.1.  Addr-Spec Specification

       An addr-spec is a specific Internet identifier that contains a
       locally interpreted string followed by the at-sign character ("@",
       ASCII value 64) followed by an Internet domain.  The locally
       interpreted string is either a quoted-string or a dot-atom.  If the
       string can be represented as a dot-atom (that is, it contains no
       characters other than atext characters or "." surrounded by atext
       characters), then the dot-atom form SHOULD be used and the quoted-
       string form SHOULD NOT be used.  Comments and folding white space
       SHOULD NOT be used around the "@" in the addr-spec.
     */

    // addr-spec       =       local-part "@" domain
    private[EmailParser] def addr_spec: Parser[Email] = local_part ~ "@" ~ domain ^^ { case local_part ~ a ~ domain => ValidEmail(local_part, domain) }
  }
}
