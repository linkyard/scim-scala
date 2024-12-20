package ch.linkyard.scim.model

import cats.implicits.*
import ch.linkyard.scim.model.Filter.AttributePath
import ch.linkyard.scim.model.Filter.AttributeSelector
import ch.linkyard.scim.model.Filter.FilteredAttributePath
import ch.linkyard.scim.model.PatchOp.Operation
import io.circe.Json
import io.circe.optics.JsonPath
import io.circe.optics.JsonPath.*

/** RFC 7644 3.5.2 */
case class PatchOp(
  Operations: Seq[Operation],
  schemas: Seq[Schema] = Seq(Schema.PatchOp),
) extends RootModel:
  def applyTo(defaultSchema: Schema)(json: Json): Either[String, Json] =
    Operations.foldLeft[Either[String, Json]](Right(json))((acc, op) => acc.flatMap(op.applyTo(defaultSchema)))

object PatchOp:
  private type EitherString[+A] = Either[String, A]

  sealed trait OperationType:
    private[PatchOp] def execute(on: Json, context: OperationContext): Either[String, Json]

  object OperationType:
    case object Add extends OperationType:
      private[PatchOp] def execute(on: Json, context: OperationContext): Either[String, Json] = context.path match
        case None =>
          addIt(root)(on, context.value)
        case Some(ap @ AttributePath(name, _, None)) =>
          val jsonPath = ap.basePath(context.defaultSchema)
          if jsonPath.json.isEmpty(on) then {
            val v = if context.value.isArray then context.value else Json.arr(context.value)
            on.asObject.map(_.add(name, v))
              .map(Json.fromJsonObject)
              .map(Right.apply)
              .getOrElse(Left("entity must be on object"))
          } else
            addIt(ap.basePath(context.defaultSchema))(on, context.value)
        case Some(ap @ AttributePath(name, _, Some(subattr))) =>
          val jsonPath = ap.basePath(context.defaultSchema)
          if jsonPath.json.isEmpty(on) then {
            val v = Json.obj(subattr -> context.value)
            on.asObject.map(_.add(name, v))
              .map(Json.fromJsonObject)
              .map(Right.apply)
              .getOrElse(Left("entity must be on object"))
          } else
            jsonPath.json.modifyA[EitherString](_.asObject
              .toRight("adding a sub attribute is only supported on complex values")
              .map(o => o.add(subattr, context.value))
              .map(Json.fromJsonObject))(on)
        case Some(FilteredAttributePath(_, _, _, _)) =>
          Left("add with filter is not supported")

      private def addIt(jsonPath: JsonPath)(on: Json, value: Json): Either[String, Json] =
        jsonPath.json.modifyA[EitherString] { json =>
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
                if value.isNull then array
                else array.appended(value)
              }
              Right(Json.fromValues(updated))
            },
          )
        }(on)
    end Add

    case object Remove extends OperationType:
      private[PatchOp] def execute(on: Json, context: OperationContext): Either[String, Json] = (context.path match
        case None => Left("no target")
        case Some(ap @ AttributePath(_, _, None)) =>
          if context.value.isNull then Right(ap.basePath(context.defaultSchema).json.modify(_ => Json.Null)(on))
          else
            removeFiltered(
              ap.basePath(context.defaultSchema),
              v => context.value.asArray.map(_.contains(v)).getOrElse(v == context.value),
            )(on)
        case Some(ap @ AttributePath(_, _, Some(sub))) =>
          if context.value.isNull then removeOnSub(ap.basePath(context.defaultSchema), sub, _ => true)(on)
          else
            removeOnSub(
              ap.basePath(context.defaultSchema),
              sub,
              v => context.value.asArray.map(_.contains(v)).getOrElse(v == context.value),
            )(on)
        case Some(ap @ FilteredAttributePath(_, filter, _, None)) =>
          removeFiltered(ap.basePath(context.defaultSchema), filter.evaluate(_, context.defaultSchema))(on)
        case Some(ap @ FilteredAttributePath(_, filter, _, Some(sub))) =>
          removeOnSub(ap.basePath(context.defaultSchema), sub, filter.evaluate(_, context.defaultSchema))(on)
      ).map(_.dropNullValues)

      private def removeFiltered(jsonPath: JsonPath, filter: Json => Boolean)(on: Json): Either[String, Json] =
        jsonPath.json.modifyA[EitherString](_.fold(
          jsonNull = Right(Json.Null),
          jsonBoolean = _ => Left("filtered removal on boolean type is illegal"),
          jsonNumber = _ => Left("filtered removal on number type is illegal"),
          jsonString = _ => Left("filtered removal on string type is illegal"),
          jsonObject = _ => Left("filtered removal on object type is illegal"),
          jsonArray = arr => Right(Json.fromValues(arr.filterNot(filter))),
        ))(on)

      private def removeOnSub(
        jsonPath: JsonPath,
        sub: String,
        filter: Json => Boolean,
      )(on: Json): Either[String, Json] =
        jsonPath.json.modifyA[EitherString](_.fold(
          jsonNull = Right(Json.Null),
          jsonBoolean = _ => Left("expected complex object but was boolean"),
          jsonNumber = _ => Left("expected complex object but was number"),
          jsonString = _ => Left("expected complex object but was string"),
          jsonObject = obj => Right(Json.fromJsonObject(obj.remove(sub))),
          jsonArray = arr =>
            arr.traverse[EitherString, Json] { el =>
              if filter(el) then
                el.asObject.map(_.remove(sub)).map(Json.fromJsonObject).toRight("expected complex object")
              else Right(el)
            }.map(Json.fromValues),
        ))(on)
    end Remove

    case object Replace extends OperationType:
      private[PatchOp] def execute(on: Json, context: OperationContext): Either[String, Json] = context.path match
        case None =>
          val f = root.json.modify(_.deepMerge(context.value))
          Right(f(on))
        case Some(ap @ AttributePath(_, _, None)) =>
          Right(ap.basePath(context.defaultSchema).json.modify(_ => context.value)(on))
        case Some(ap @ AttributePath(_, _, Some(sub))) =>
          replaceOnSub(ap.basePath(context.defaultSchema), sub)(on, context.value)
        case Some(ap @ FilteredAttributePath(_, filter, _, None)) =>
          replaceOnBaseWithFilter(ap.basePath(context.defaultSchema), filter)(on, context.value)
        case Some(ap @ FilteredAttributePath(_, filter, _, Some(sub))) =>
          replaceOnSubWithFilter(ap.basePath(context.defaultSchema), filter, sub)(on, context.value)

      private def replaceOnBaseWithFilter(
        jsonPath: JsonPath,
        filter: Filter,
      )(on: Json, value: Json): Either[String, Json] =
        jsonPath.json.modifyA[EitherString](json =>
          json.asArray.toRight("filter only supported on array values")
            .map(arr => arr.map(e => if filter.evaluate(e) then value else e))
            .map(Json.fromValues)
        )(on)

      private def replaceOnSub(jsonPath: JsonPath, sub: String)(on: Json, value: Json): Either[String, Json] =
        jsonPath.json.modifyA[EitherString] { json =>
          json.fold(
            jsonNull = Right(Json.obj(sub -> value)),
            jsonBoolean = _ => Left("expected complex object but was boolean"),
            jsonNumber = _ => Left("expected complex object but was number"),
            jsonString = _ => Left("expected complex object but was string"),
            jsonObject = obj => Right(Json.fromJsonObject(obj.add(sub, value))),
            jsonArray = _ => Right(root.each.selectDynamic(sub).json.modify(_ => value)(json)),
          )
        }(on)

      private def replaceOnSubWithFilter(jsonPath: JsonPath, filter: Filter, sub: String)(
        on: Json,
        value: Json,
      ): Either[String, Json] = {
        def update(element: Json): Either[String, Json] =
          element.asObject.map(_.add(sub, value))
            .map(Json.fromJsonObject)
            .map(Right.apply)
            .getOrElse(Left("sub attribute only supported on complex object"))

        jsonPath.json.modifyA[EitherString](json =>
          json.asArray.toRight("filter only supported on array values")
            .flatMap(arr => arr.traverse(e => if filter.evaluate(e) then update(e) else Right(e)))
            .map(Json.fromValues)
        )(on)
      }
    end Replace
  end OperationType

  private case class OperationContext(
    path: Option[AttributeSelector],
    pathRendered: String,
    value: Json,
    defaultSchema: Schema,
  )

  case class Operation(op: OperationType, path: Option[AttributeSelector], value: Option[Json]):
    def applyTo(defaultSchema: Schema)(json: Json): Either[String, Json] =
      val context = OperationContext(
        path = path,
        defaultSchema = defaultSchema,
        value = value.getOrElse(Json.Null),
        pathRendered = path.map(_.render).getOrElse("/"),
      )
      op.execute(json, context)
  end Operation
end PatchOp
