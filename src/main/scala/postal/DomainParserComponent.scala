package postal

import scala.util.parsing.combinator.RegexParsers
import java.util.Hashtable
import javax.naming.directory.InitialDirContext
import scala.util.Try

trait DomainParserComponent {
  this: AtomParserComponent
    with FoldingWhiteSpaceAndCommentsParserComponent
    with ObsoleteDefinitionsParserComponent
    with LengthComponent
    with CoreRulesParserComponent
    with AddressLiteralsParserComponent =>

  trait DomainParser extends RegexParsers {

    protected def domain: Parser[String]

    protected def domain_literal: Parser[String]

    protected def dtext: Parser[String]

  }

  trait BaseDomainParser extends DomainParser {
    this: DotAtomParser with FoldingWhiteSpaceAndCommentsParser with ObsoleteDomainParser =>
    /*
       RFC 5321

       4.5.3.1.2.  Domain

       The maximum total length of a domain name or number is 255 octets.
     */
    // domain          =       dot-atom / domain-literal / obs-domain
    protected def domain: Parser[String] = dot_atom ||| domain_literal ||| obs_domain

    // domain-literal  =   [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
    protected def domain_literal: Parser[String] = opt(CFWS) ~ "[" ~ rep(opt_FWS_and_dtext) ~ opt(FWS) ~ "]" ~ opt(CFWS) ^^ {
      case Some(lc) ~ lb ~ ds ~ Some(f) ~ rb ~ Some(rc) =>
        lc + lb + ds.mkString + f + rb + rc
      case None ~ lb ~ ds ~ Some(f) ~ rb ~ Some(rc) =>
        lb + ds.mkString + f + rb + rc
      case Some(lc) ~ lb ~ ds ~ None ~ rb ~ Some(rc) =>
        lc + lb + ds.mkString + rb + rc
      case Some(lc) ~ lb ~ ds ~ Some(f) ~ rb ~ None =>
        lc + lb + ds.mkString + f + rb
      case None ~ lb ~ ds ~ None ~ rb ~ Some(rc) =>
        lb + ds.mkString + rb + rc
      case None ~ lb ~ ds ~ Some(f) ~ rb ~ None =>
        lb + ds.mkString + f + rb
      case Some(lc) ~ lb ~ ds ~ None ~ rb ~ None =>
        lc + lb + ds.mkString + rb
      case None ~ lb ~ ds ~ None ~ rb ~ None =>
        lb + ds.mkString + rb
    }

    private def opt_FWS_and_dtext: Parser[String] = opt(FWS) ~ dtext ^^ {
      case Some(f) ~ d => f + d
      case None ~ d => d
    }

    /*
       dtext           =   %d33-90 /          ; Printable US-ASCII
                           %d94-126 /         ;  characters not including
                           obs-dtext          ;  "[", "]", or "\"
     */
    protected def dtext: Parser[String] = "[\\u0021-\\u005A]".r | "[\\u005E-\\u007E]".r | obs_dtext
  }

  trait AddressLiteralRestriction extends DomainParser {
    this: AddressLiteralsParser =>

    protected abstract override def dtext: Parser[String] = IPv4_address_literal ||| IPv6_address_literal
  }

  trait DomainLengthRestriction extends DomainParser {
    /*
       RFC 5321

       4.5.3.1.1.  Local-part

       The maximum total length of a user name or other local-part is 64
       octets.
     */
    protected abstract override def domain: Parser[String] = super.domain filter Length.lessThanOrEqualTo255
  }

  trait DomainExistsRestriction extends ObsoleteDomainParser with DotAtomParser {

    protected abstract override def dot_atom: Parser[String] = super.dot_atom filter exists

    protected abstract override def obs_domain: Parser[String] = super.obs_domain filter exists

    /*
       Perform a check to see if the supplied domain exists.
     */
    private def exists(domain: String): Boolean = {
      val env = new Hashtable[String, String]
      env.put("java.naming.factory.initial", "com.sun.jndi.dns.DnsContextFactory")
      val context = new InitialDirContext(env)

      val attributes = Try(context.getAttributes(domain, Array("MX")))

      context.close()

      attributes.isSuccess
    }
  }
}
