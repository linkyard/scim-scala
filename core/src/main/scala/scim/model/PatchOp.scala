package scim.model

import io.circe.{Json, JsonObject}
import cats.implicits._
import scim.model.Filter.AttributeSelector
import scim.model.PatchOp.Operation
import io.circe.optics.JsonPath._

/** RFC 7644 3.5.2 */
case class PatchOp(
  Operations: Seq[Operation],
  schemas: Seq[Schema] = Seq(Schema.PatchOp)) extends RootModel {

  def applyTo(defaultSchema: Schema)(json: Json): Either[String, Json] = {
    Operations.foldLeft[Either[String, Json]](Right(json))((acc, op) => acc.flatMap(op.applyTo(defaultSchema)))
  }
}

object PatchOp {
  sealed trait OperationType
  object OperationType {
    case object Add extends OperationType
    case object Remove extends OperationType
    case object Replace extends OperationType
  }

  case class Operation(op: OperationType, path: Option[AttributeSelector], value: Json) {
    private def executeOnArray(arr: Vector[Json]): Either[String, Json] = value.asArray match {
      case Some(value) => Right(Json.arr((arr ++ value): _*))
      case None => Left("can only add an array to an array")
    }
    private def executeOnObject(obj: JsonObject): Either[String, Json] = value.asObject match {
      case Some(value) =>
        val updated = value.toIterable.foldLeft(obj)((acc, v) => acc.add(v._1, v._2))
        Right(Json.fromJsonObject(updated))
      case None => Left("can only add an object to an object")
    }

    def applyTo(defaultSchema: Schema)(json: Json): Either[String, Json] = {
      val pathRender = path.map(_.render).getOrElse("/")
      // TODO switch on operation...
      val result = path.map(_.jsonPathSingle(defaultSchema)).getOrElse(root).json
        .modifyF[Either[String, ?]] { j =>
          None
            .orElse(j.asArray.map(executeOnArray))
            .orElse(j.asObject.map(executeOnObject))
            .getOrElse(Left(s"add not supported for path ${pathRender}"))
        }(json)
      result
    }
  }
}
