package scim.spi

sealed trait SpiError

object SpiError {
  sealed trait CreationError extends SpiError

  case object AlreadyExists extends SpiError with CreationError
  type AlreadyExists = AlreadyExists.type
  case class MalformedData(details: String) extends SpiError with CreationError
  case class MissingData(details: String) extends SpiError with CreationError
}
