import postal.Email
import org.specs2.mutable.Specification

class EmailSpec extends Specification {
  "The EmailAddress validator" should {
    "fail to validate an empty email address" in {
      Email.isValid("") must beFalse
    }

    "fail to validate an email address without an ampersat" in {
      Email.isValid("test") must beFalse
    }

    "fail to validate an email address without a domain" in {
      Email.isValid("test@") must beFalse
    }

    "fail to validate an email address without a local part or domain" in {
      Email.isValid("@") must beFalse
    }

    "fail to validate an email address without a local part" in {
      Email.isValid("@example.com") must beFalse
    }

    "successfully validate an email address with both a local part and domain" in {
      Email.isValid("test@example") must beTrue
    }

    "successfully validate an email address with a domain suffix" in {
      Email.isValid("test@example.com") must beTrue
    }

    "successfully validate an email address with a local part shorter than or equal to 64 characters" in {
      Email.isValid("0123456789012345678901234567890123456789012345678901234567890123@example.com") must beTrue
    }

    "fail to validate an email address with a local part longer than 64 characters" in {
      Email.isValid("01234567890123456789012345678901234567890123456789012345678901234@example.com") must beFalse
    }

    "successfully validate an email address with a domain shorter than or equals to 255 characters" in {
      Email.isValid("test@012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234") must beTrue
    }

    "fail to validate an email address with a domain longer than 255 characters" in {
      Email.isValid("test@0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345") must beFalse
    }

    "fail to validate an email address with two ampersats" in {
      Email.isValid("test@@example.com") must beFalse
    }

    "fail to validate an email address with a local part beginning with a dot" in {
      Email.isValid(".test@example.com") must beFalse
    }

    "fail to validate an email address with a domain beginning with a dot" in {
      Email.isValid("test@.example.com") must beFalse
    }

    "fail to validate an email address with local part ending with a dot" in {
      Email.isValid("test.@example.com") must beFalse
    }

    "fail to validate an email address with a domain ending with a dot" in {
      Email.isValid("test@example.com.") must beFalse
    }

    "fail to validate an email address with a local part containing sequential dots" in {
      Email.isValid("local..test@example.com") must beFalse
    }

    "fail to validate an email address with a domain containing sequential dots" in {
      Email.isValid("test@domain..example.com") must beFalse
    }

    "successfully validate an email address with a local part containing a dot" in {
      Email.isValid("local.test@example.com") must beTrue
    }

    "successfully validate an email address with legal symbols" in {
      Email.isValid("!#$%&`*+/=?^{|}~@example.com") must beTrue
    }

    "successfully validate an email address with a local part containing a quoted pair" in {
      Email.isValid("\"test\"@example.com") must beTrue
    }

    "fail to validate an email address with a local part containing multiple quoted pairs" in {
      Email.isValid("\"te\"\"st\"@example.com") must beFalse
    }

    "fail to validate an email address with a local part with text before a quoted pair" in {
      Email.isValid("te\"st\"@example.com") must beFalse
    }

    "fail to validate an email address with a local part with text after a quoted pair" in {
      Email.isValid("\"te\"st@example.com") must beFalse
    }

    "successfully validate an email address with a local part containing an empty quoted pair" in {
      Email.isValid("\"\"@example.com") must beTrue
    }

    "successfully validate an email address with a local part containing a quoted pair with an ampersat" in {
      Email.isValid("\"test@\"@example.com") must beTrue
    }

    "fail to validate an email address without matching quotes" in {
      Email.isValid("\"test@example.com") must beFalse
    }

    "fail to validate an email address without matching quotes due to being escaped" in {
      Email.isValid("\"test\\\"@example.com") must beFalse
    }

    "fail to validate an email address with a domain containing a quoted pair" in {
      Email.isValid("test@\"example\".com") must beFalse
    }

    "successfully validate an email address with a local part containing a comment" in {
      Email.isValid("test(comment)@example.com") must beTrue
    }

    "successfully validate an email address with a local part containing nested comments" in {
      Email.isValid("test(co(mm)ent)@example.com") must beTrue
    }

    "fail to validate an email address with a local part with an un-closed comment" in {
      Email.isValid("test(comment@example.com") must beFalse
    }

    "successfully validate an email address with a domain containing a comment immediately following the ampersat" in {
      Email.isValid("test@(comment)example.com") must beTrue
    }

    "fail to validate an email address with a domain containing an unclosed comment immediately following the ampersat" in {
      Email.isValid("test@(commentexample.com") must beFalse
    }

    "successfully validate an email address with a domain containing a comment not immediately following the ampersat" in {
      Email.isValid("test@example(comment).com") must beTrue
    }

    "successfully validate an email address with a local part containing an escaped sequence in a quoted pair" in {
      Email.isValid("\"te\\st\"@example.com") must beTrue
    }

    "successfully validate an email address with a domain that is an IP address" in {
      Email.isValid("test@255.255.255.255") must beTrue
    }

    "successfully validate an email address with a domain that is an IP address within brackets" in {
      Email.isValid("test@[255.255.255.255]") must beTrue
    }

    "fail to validate an email address with a domain that is a domain literal without a closing bracket" in {
      Email.isValid("test@[255.255.255.255") must beFalse
    }

    "successfully validate an email address with a domain that is an IP address within brackets preceded by a comment" in {
      Email.isValid("test@(comment)[255.255.255.255]") must beTrue
    }

    "fail to validate an email address with a domain that is an IP address within brackets with other text" in {
      Email.isValid("test@example[255.255.255.255]") must beFalse
    }

    "fail to validate an email address with a domain that is an IP address within brackets with too few segments" in {
      Email.isValid("test@[255.255.255]") must beFalse
    }

    "fail to validate an email address with a domain that is an IP address within brackets with too many segments" in {
      Email.isValid("test@[255.255.255.255.255]") must beFalse
    }

    "fail to validate an email address with a domain that is an IP address within brackets with a segment outside the allowed range" in {
      Email.isValid("test@[255.255.255.256]") must beFalse
    }

    "successfully validate an email address with a local part containing an escaped carriage return in a quoted pair" in {
      Email.isValid("\"te\\\rst\"@example.com") must beTrue
    }

    "fail to validate an email address with a local part containing an illegal character in a quoted pair" in {
      Email.isValid("\"test\0\"@example.com") must beFalse
    }

    "successfully validate an email address with a local part containing a quoted pair where the length is equal to 64 characters including the quotes" in {
      Email.isValid("\"01234567890123456789012345678901234567890123456789012345678901\"@example.com") must beTrue
    }

    "fail to validate an email address with a local part containing a quoted pair where the length is greater than 64 characters including the quotes" in {
      Email.isValid("\"012345678901234567890123456789012345678901234567890123456789012\"@example.com") must beFalse
    }

    "successfully validate an email address with whitespace around the local part" in {
      Email.isValid(" test @example.com") must beTrue
    }

    "successfully validate an email address with whitespace around the domain" in {
      Email.isValid("test@ example.com ") must beTrue
    }

    "successfully validate an email address with a local part containing whitespace" in {
      Email.isValid("test . test@example.com") must beTrue
    }

    "successfully validate an email address with a local part beginning with a newline" in {
      Email.isValid("\r\n test@example.com") must beTrue
    }

    "successfully validate an email address with a local part containing one line of only whitespace" in {
      Email.isValid("\r\n \r\n test@example.com") must beFalse
    }

    "fail to validate an email address with a domain that is an IPv6 address within brackets without a tag" in {
      Email.isValid("test@[1111:2222:3333:4444:5555:6666:7777:8888]") must beFalse
    }

    "fail to validate an email address with a domain that is an IPv6 address within brackets with too few segments" in {
      Email.isValid("test@[IPv6:1111:2222:3333:4444:5555:6666:7777]") must beFalse
    }

    "fail to validate an email address with a domain that is an IPv6 address within brackets with too many segments" in {
      Email.isValid("test@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888:9999]") must beFalse
    }

    "fail to validate an email address with a domain that is an IPv6 address within brackets with illegal characters" in {
      Email.isValid("test@[IPv6:1111:2222:3333:4444:5555:6666:7777:888G]") must beFalse
    }

    "successfully validate an email address with a domain that is an IPv6 address within brackets" in {
      Email.isValid("test@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888]") must beTrue
    }

    "successfully validate an email address with a domain that is an IPv6 address within brackets containing a '::' that elides one zero group" in {
      Email.isValid("test@[IPv6:1111:2222:3333:4444:5555:6666::8888]") must beTrue
    }

    "fail to validate an email address with a domain that is an IPv6 address within brackets containing a '::' with too many segments" in {
      Email.isValid("test@[IPv6:1111:2222:3333:4444:5555:6666::7777:8888]") must beFalse
    }

    "successfully validate an email address with a domain that is an IPv6 address within brackets containing a '::' that elides multiple zero groups" in {
      Email.isValid("test@[IPv6:1111:2222:3333:4444:5555::8888]") must beTrue
    }

    "fail to validate an email address with a domain that is an IPv6 address within brackets beginning with ':'" in {
      Email.isValid("test@[IPv6::2222:3333:4444:5555:7777:8888]") must beFalse
    }

    "successfully validate an email address with a domain that is an IPv6 address within brackets beginning with '::'" in {
      Email.isValid("test@[IPv6:::2222:3333:4444:5555:7777:8888]") must beTrue
    }

    "fail to validate an email address with a domain that is an IPv6 address within brackets with more than one '::'" in {
      Email.isValid("test@[IPv6:1111::3333:4444:5555:6666::8888]") must beFalse
    }

    "successfully validate an email address with a domain that is an IPv6 address within brackets containing only zero groups" in {
      Email.isValid("test@[IPv6:::]") must beTrue
    }

    "fail to validate an email address with a domain that is an IPv6v4 address within brackets with too few groups" in {
      Email.isValid("test@[IPv6:1111:2222:3333:4444:5555:255.255.255.255]") must beFalse
    }

    "successfully validate an email address with a domain that is an IPv6v4 address with six groups" in {
      Email.isValid("test@[IPv6:1111:2222:3333:4444:5555:6666:255.255.255.255]") must beTrue
    }

    "fail to validate an email address with a domain that is an IPv6v4 address with too many groups" in {
      Email.isValid("test@[IPv6:1111:2222:3333:4444:5555:6666:7777:255.255.255.255]") must beFalse
    }

    "successfully validate an email address with a domain that is an IPv6v4 address with a '::'" in {
      Email.isValid("test@[IPv6:1111:2222:3333:4444::255.255.255.255]") must beTrue
    }

    "fail to validate an email address with a domain that is an IPv6v4 address with a '::' and too many groups" in {
      Email.isValid("test@[IPv6:1111:2222:3333:4444:5555:6666::255.255.255.255]") must beFalse
    }

    "fail to validate an email address with too many '::'" in  {
      Email.isValid("test@[IPv6:1111:2222:3333:4444::::255.255.255.255]") must beFalse
    }

    "fail to validate an email address with a domain that is an IPv6v4 address with only an IPv4 section" in {
      Email.isValid("test@[IPv6::255.255.255.255]") must beFalse
    }

    "successfully validate an email address with folding white space in the local part" in {
      Email.isValid(" test @example.com") must beTrue
    }

    "successfully validate an email address with folding white space in the domain" in {
      Email.isValid("test@ example .com") must beTrue
    }

    "successfully validate an email address with folding white space in the middle of the local part" in {
      Email.isValid("test . test@example.com") must beTrue
    }

    "fail to validate an email address with a domain that ends with a hyphen" in {
      Email.isValid("test@example.com-") must beFalse
    }
  }
}
