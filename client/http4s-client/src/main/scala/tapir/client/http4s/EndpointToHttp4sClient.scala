package tapir.client.http4s

import java.net.URI

import cats.effect.ConcurrentEffect
import cats.implicits._
import org.http4s.Status.{ClientError, Successful}
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.headers.{Accept, MediaRangeAndQValue}
import org.http4s.{EntityDecoder, Method, Request, Uri}
import tapir._
import tapir.client.UriExtractor

import scala.concurrent.ExecutionContext

class EndpointToHttp4sClient[F[_]: ConcurrentEffect](clientOptions: Http4sClientOptions)(implicit ec: ExecutionContext) {
  def toHttp4sRequest[I, E, O](
      e: Endpoint[I, E, O, Nothing],
      baseUri: Uri
  )(
      implicit outputDecoder: EntityDecoder[F, O],
      errorDecoder: EntityDecoder[F, E]
  ): I => F[Either[E, O]] = { params =>
    val jBaseUri = URI.create(baseUri.renderString)
    val (uri, req) = UriExtractor.extractUri(jBaseUri, e, params)

    val request = Request[F](
      Method.fromString(req.method).getOrElse(Method.GET),
      uri = Uri.unsafeFromString(uri.toString)
    )

    BlazeClientBuilder[F](ec).resource.use { client =>
      expectEither[E, O](client, request)
    }
  }

  private def expectEither[E, O](
      client: Client[F],
      req: Request[F]
  )(implicit O: EntityDecoder[F, O], E: EntityDecoder[F, E]): F[Either[E, O]] = {
    val r = if (O.consumes.nonEmpty) {
      val m = O.consumes.toList
      req.putHeaders(Accept(MediaRangeAndQValue(m.head), m.tail.map(MediaRangeAndQValue(_)): _*))
    } else req
    client.fetch(r) {
      case Successful(resp) =>
        O.decode(resp, strict = false).leftWiden[Throwable].rethrowT.map(Right(_))
      case ClientError(resp) =>
        E.decode(resp, strict = false).leftWiden[Throwable].rethrowT.map(Left(_))
    }
  }
}
