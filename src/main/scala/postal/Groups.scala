package postal

trait Groups {
  def noMoreThan6: String => Boolean = max(6)

  def noMoreThan4: String => Boolean = max(4)

  private def max(count: Int)(value: String) = value.count(_ == ':') <= count + 1
}

object Groups extends Groups
