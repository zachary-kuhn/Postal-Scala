package postal

trait LengthComponent {
  trait Length {
    def lessThanOrEqualTo(length: Int)(localPart: String): Boolean = localPart.length <= length

    def lessThanOrEqualTo64: String => Boolean = lessThanOrEqualTo(64)

    def lessThanOrEqualTo255: String => Boolean = lessThanOrEqualTo(255)
  }

  object Length extends Length
}
