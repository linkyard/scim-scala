package ch.linkyard.scim.model

enum SortOrder:
  case Ascending
  case Descending

  def asString: String = this match
    case Ascending  => "ascending"
    case Descending => "descending"

object SortOrder:
  def default: SortOrder = Ascending

  def parse(string: String): Either[String, SortOrder] = string.trim match
    case "ascending"  => Right(Ascending)
    case "descending" => Right(Descending)
    case ""           => Right(Ascending)
    case other        => Left(s"$other ist not a valid sortOrder")
