package scim.model

import cats.implicits._
import io.circe.Json
import io.circe.optics.JsonPath
import io.circe.optics.JsonPath._
import scim.model.Filter.{AttributePath, AttributeSelector, FilteredAttributePath}
import scim.model.PatchOp.Operation

/** RFC 7644 3.5.2 */
case class PatchOp(
  Operations: Seq[Operation],
  schemas: Seq[Schema] = Seq(Schema.PatchOp)) extends RootModel {

  def applyTo(defaultSchema: Schema)(json: Json): Either[String, Json] = {
    Operations.foldLeft[Either[String, Json]](Right(json))((acc, op) => acc.flatMap(op.applyTo(defaultSchema)))
  }
}

object PatchOp {
  sealed trait OperationType {
    private[PatchOp] def execute(on: Json, context: OperationContext): Either[String, Json]
  }
  object OperationType {
    case object Add extends OperationType {
      private[PatchOp] def execute(on: Json, context: OperationContext): Either[String, Json] = {
        context.path match {
          case None =>
            addIt(root)(on, context.value)
          case Some(ap@AttributePath(name, _, None)) =>
            addIt(ap.basePath(context.defaultSchema))(on, context.value)
          case Some(ap@AttributePath(name, _, Some(_))) =>
            Left("adding a sub attribute is not supported")
          case Some(FilteredAttributePath(_, _, _, _)) =>
            Left("add with filter is not supported")
        }
      }

      private def addIt(jsonPath: JsonPath)(on: Json, value: Json): Either[String, Json] = {
        jsonPath.json.modifyF[Either[String, *]] { json =>
          json.fold(
            jsonNull = Right(value),
            jsonBoolean = _ => Right(value),
            jsonNumber = _ => Right(value),
            jsonString = _ => Right(value),
            jsonObject = obj =>
              value.asObject.map(_.toIterable.foldLeft(obj)((acc, v) => acc.add(v._1, v._2)))
                .map(Json.fromJsonObject)
                .map(Right.apply)
                .getOrElse(Left(s"not a complex object ${jsonPath.toString}")),
            jsonArray = array => {
              val updated = value.asArray.map(array.++).getOrElse {
                if (value.isNull) array
                else array.appended(value)
              }
              Right(Json.fromValues(updated))
            }
          )
        }(on)
      }
    }

    case object Remove extends OperationType {
      private[PatchOp] def execute(on: Json, context: OperationContext): Either[String, Json] = {
        (context.path match {
          case None => Left("no target")
          case Some(ap@AttributePath(_, _, None)) =>
            Right(ap.basePath(context.defaultSchema).json.modify(_ => Json.Null)(on))
          case Some(ap@AttributePath(_, _, Some(sub))) =>
            removeOnSub(ap.basePath(context.defaultSchema), sub, _ => true)(on)
          case Some(ap@FilteredAttributePath(_, filter, _, None)) =>
            removeFiltered(ap.basePath(context.defaultSchema), filter.evaluate(_, context.defaultSchema))(on)
          case Some(ap@FilteredAttributePath(_, filter, _, Some(sub))) =>
            removeOnSub(ap.basePath(context.defaultSchema), sub, filter.evaluate(_, context.defaultSchema))(on)
        }).map(_.dropNullValues)
      }

      private def removeFiltered(jsonPath: JsonPath, filter: Json => Boolean)(on: Json): Either[String, Json] = {
        jsonPath.json.modifyF[Either[String, ?]](_.fold(
          jsonNull = Right(Json.Null),
          jsonBoolean = _ => Left("filtered removal on boolean type is illegal"),
          jsonNumber = _ => Left("filtered removal on number type is illegal"),
          jsonString = _ => Left("filtered removal on string type is illegal"),
          jsonObject = _ => Left("filtered removal on object type is illegal"),
          jsonArray = arr => Right(Json.fromValues(arr.filterNot(filter)))
        ))(on)
      }

      private def removeOnSub(jsonPath: JsonPath, sub: String, filter: Json => Boolean)(on: Json): Either[String, Json] = {
        jsonPath.json.modifyF[Either[String, *]](_.fold(
          jsonNull = Right(Json.Null),
          jsonBoolean = _ => Left("expected complex object but was boolean"),
          jsonNumber = _ => Left("expected complex object but was number"),
          jsonString = _ => Left("expected complex object but was string"),
          jsonObject = obj => Right(Json.fromJsonObject(obj.remove(sub))),
          jsonArray = arr => arr.traverse[Either[String, ?], Json] { el =>
            if (filter(el)) el.asObject.map(_.remove(sub)).map(Json.fromJsonObject).toRight("expected complex object")
            else Right(el)
          }.map(Json.fromValues)
        ))(on)
      }
    }

    case object Replace extends OperationType {
      private[PatchOp] def execute(on: Json, context: OperationContext): Either[String, Json] = {
        context.path match {
          case None =>
            Right(root.json.modify(_ => context.value)(on))
          case Some(ap@AttributePath(name, _, None)) =>
            Right(ap.basePath(context.defaultSchema).json.modify(_ => context.value)(on))
          case Some(ap@AttributePath(name, _, Some(sub))) =>
            replaceOnSub(ap.basePath(context.defaultSchema), sub)(on, context.value)
          case Some(ap@FilteredAttributePath(_, filter, _, None)) =>
            replaceOnBaseWithFilter(ap.basePath(context.defaultSchema), filter)(on, context.value)
          case Some(ap@FilteredAttributePath(_, filter, _, Some(sub))) =>
            replaceOnSubWithFilter(ap.basePath(context.defaultSchema), filter, sub)(on, context.value)
        }
      }

      private def replaceOnBaseWithFilter(jsonPath: JsonPath, filter: Filter)(on: Json, value: Json): Either[String, Json] = {
        def update(element: Json): Either[String, Json] = Right(on)

        jsonPath.json.modifyF[Either[String, *]](json =>
          json.asArray.toRight("filter only supported on array values")
            .map(arr => arr.map(e => if (filter.evaluate(e)) value else e))
            .map(Json.fromValues)
        )(on)
      }

      private def replaceOnSub(jsonPath: JsonPath, sub: String)(on: Json, value: Json): Either[String, Json] = {
        jsonPath.json.modifyF[Either[String, *]] { json =>
          json.fold(
            jsonNull = Right(Json.obj(sub -> value)),
            jsonBoolean = _ => Left("expected complex object but was boolean"),
            jsonNumber = _ => Left("expected complex object but was number"),
            jsonString = _ => Left("expected complex object but was string"),
            jsonObject = obj => Right(Json.fromJsonObject(obj.add(sub, value))),
            jsonArray = _ => Right(root.each.selectDynamic(sub).json.modify(_ => value)(json))
          )
        }(on)
      }

      private def replaceOnSubWithFilter(jsonPath: JsonPath, filter: Filter, sub: String)(on: Json, value: Json): Either[String, Json] = {
        def update(element: Json): Either[String, Json] = {
          element.asObject.map(_.add(sub, value))
            .map(Json.fromJsonObject)
            .map(Right.apply)
            .getOrElse(Left("sub attribute only supported on complex object"))
        }

        jsonPath.json.modifyF[Either[String, *]](json =>
          json.asArray.toRight("filter only supported on array values")
            .flatMap(arr => arr.traverse(e => if (filter.evaluate(e)) update(e) else Right(e)))
            .map(Json.fromValues)
        )(on)
      }
    }
  }

  private case class OperationContext(
    path: Option[AttributeSelector],
    pathRendered: String,
    value: Json,
    defaultSchema: Schema,
  )

  case class Operation(op: OperationType, path: Option[AttributeSelector], value: Option[Json]) {

    def applyTo(defaultSchema: Schema)(json: Json): Either[String, Json] = {
      val context = OperationContext(
        path = path, defaultSchema = defaultSchema,
        value = value.getOrElse(Json.Null),
        pathRendered = path.map(_.render).getOrElse("/"))
      op.execute(json, context)
    }
  }
}
