package tapir.client.http4s

import cats.effect.ConcurrentEffect
import org.http4s.{EntityDecoder, Uri}
import tapir.Endpoint

import scala.concurrent.ExecutionContext

trait TapirHttp4sClient {
  implicit class RichEndpoint[I, E, O](e: Endpoint[I, E, O, Nothing])(implicit ec: ExecutionContext) {
    def toHttp4sRequest[F[_]: ConcurrentEffect](baseUri: Uri)(
        implicit clientOptions: Http4sClientOptions,
        outputDecoder: EntityDecoder[F, O],
        inputDecoder: EntityDecoder[F, E]
    ): I => F[Either[E, O]] =
      new EndpointToHttp4sClient(clientOptions).toHttp4sRequest(e, baseUri)
  }
}
