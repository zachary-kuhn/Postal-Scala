package postal

import scala.util.parsing.combinator.RegexParsers

trait AddressLiteralsParserComponent {
  this: CoreRulesParserComponent with NumberComponent =>

  trait AddressLiteralsParser extends RegexParsers with ParsersExtension {
    this: CoreRulesParser =>
    // IPv4-address-literal  = Snum 3("."  Snum)
    def IPv4_address_literal: Parser[String] = Snum ~ repN(3, dotAndSnum) ^^ {
      case s ~ ss => s + ss.mkString
    }

    private def dotAndSnum: Parser[String] = "." ~ Snum ^^ {
      case "." ~ s => "." + s
    }

    // IPv6-address-literal  = "IPv6:" IPv6-addr
    def IPv6_address_literal: Parser[String] = "IPv6:" ~ IPv6_addr ^^ {
      case "IPv6:" ~ i => "IPv6:" + i
    }

    /*
       Snum           = 1*3DIGIT
                      ; representing a decimal integer
                      ; value in the range 0 through 255
     */
    def Snum: Parser[String] = repRange(1, 3, DIGIT) ^^ {
      case digits => digits.mkString
    } filter Number.between0and255

    // IPv6-addr      = IPv6-full / IPv6-comp / IPv6v4-full / IPv6v4-comp
    def IPv6_addr: Parser[String] = IPv6_full ||| IPv6_comp ||| IPv6v4_full ||| IPv6v4_comp

    // IPv6-hex       = 1*4HEXDIG
    def IPv6_hex: Parser[String] = repRange(1, 4, HEXDIG) ^^ {
      case hs => hs.mkString
    }

    // IPv6-full      = IPv6-hex 7(":" IPv6-hex)
    def IPv6_full: Parser[String] = hexColonAndHex(7)

    /*
       IPv6-comp      = [IPv6-hex *5(":" IPv6-hex)] "::"
                      [IPv6-hex *5(":" IPv6-hex)]
                      ; The "::" represents at least 2 16-bit groups of
                      ; zeros.  No more than 6 groups in addition to the
                      ; "::" may be present.
     */
    def IPv6_comp: Parser[String] = opt(hexColonAndHexRange(5)) ~ "::" ~
      opt(hexColonAndHexRange(5)) ^^ {
      case Some(i) ~ "::" ~ Some(i2) => i + "::" + i2
      case None ~ "::" ~ Some(i2) => "::" + i2
      case Some(i) ~ "::" ~ None => i + "::"
      case None ~ "::" ~ None => "::"
    } filter Groups.noMoreThan6

    // IPv6v4-full    = IPv6-hex 5(":" IPv6-hex) ":" IPv4-address-literal
    def IPv6v4_full: Parser[String] = hexColonAndHex(5) ~ ":" ~ IPv4_address_literal ^^ {
      case i ~ ":" ~ l => i + ":" + l
    }

    /*
       IPv6v4-comp    = [IPv6-hex *3(":" IPv6-hex)] "::"
                      [IPv6-hex *3(":" IPv6-hex) ":"]
                      IPv4-address-literal
                      ; The "::" represents at least 2 16-bit groups of
                      ; zeros.  No more than 4 groups in addition to the
                      ; "::" and IPv4-address-literal may be present.
     */
    def IPv6v4_comp: Parser[String] = opt(hexColonAndHexRange(3)) ~ "::" ~
      opt(hexColonAndHexRange(3) ~ ":") ~
      IPv4_address_literal ^^ {
      case Some(i) ~ "::" ~ Some(i2 ~ ":") ~ l => i + "::" + i2 + ":" + l
      case None ~ "::" ~ Some(i2 ~ ":") ~ l => "::" + i2 + ":" + l
      case Some(i) ~ "::" ~ None ~ l => i + "::" + l
      case None ~ "::" ~ None ~ l => "::" + l
    } filter Groups.noMoreThan4

    private def hexColonAndHexRange(repetitions: Int): Parser[String] = IPv6_hex ~ opt(repRange(1, repetitions, colonAndHex)) ^^ {
      case i ~ Some(is) => i + is.mkString
      case i ~ None => i
    }

    private def hexColonAndHex(repetitions: Int): Parser[String] = IPv6_hex ~ repN(repetitions, colonAndHex) ^^ {
      case i ~ is => i + is.mkString
    }

    private def colonAndHex: Parser[String] = ":" ~ IPv6_hex ^^ {
      case ":" ~ i => ":" + i
    }
  }
}
