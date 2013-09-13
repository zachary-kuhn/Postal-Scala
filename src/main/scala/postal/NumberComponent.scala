package postal

trait NumberComponent {
  trait Number {
    def between0and255(stringRepresentation: String): Boolean = between(0)(255)(stringRepresentation)

    def between(min: Int)(max: Int)(stringRepresentation: String): Boolean = {
      val value = stringRepresentation.toInt
      value >= min && value <= max
    }
  }

  object Number extends Number
}
