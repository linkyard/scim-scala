package scim.rest

import io.circe.Json

trait Resource[F[_]] {
  type QueryParams = Map[String, String]
  type Path = Seq[String]

  def get(subPath: Path, queryParams: QueryParams): F[Response]
  def post(subPath: Path, queryParams: QueryParams, body: Json): F[Response]
  def put(subPath: Path, queryParams: QueryParams, body: Json): F[Response]
  def patch(subPath: Path, queryParams: QueryParams, body: Json): F[Response]
  def delete(subPath: Path, queryParams: QueryParams): F[Response]
}
