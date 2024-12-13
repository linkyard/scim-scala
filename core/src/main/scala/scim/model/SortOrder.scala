package scim.model

sealed trait SortOrder {
  def asString: String
}

object SortOrder {
  case object Ascending extends SortOrder {
    override def asString: String = "ascending"
  }
  case object Descending extends SortOrder {
    override def asString: String = "descending"
  }

  def default: SortOrder = Ascending

  def parse(string: String): Either[String, SortOrder] = string.trim match {
    case "ascending"  => Right(Ascending)
    case "descending" => Right(Descending)
    case ""           => Right(Ascending)
    case other        => Left(s"$other ist not a valid sortOrder")
  }
}
