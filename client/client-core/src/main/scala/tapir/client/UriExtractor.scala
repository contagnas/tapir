package tapir.client

import java.net.URI

import tapir.Codec.PlainCodec
import tapir.internal._
import tapir.model.MultiQueryParams
import tapir.{CodecForOptional, Endpoint, EndpointIO, EndpointInput, MediaType}

object UriExtractor {

  private implicit class RichUri(uri: URI) {
    def copy(
        scheme: String = uri.getScheme,
        userInfo: String = uri.getUserInfo,
        host: String = uri.getHost,
        port: Int = uri.getPort,
        path: String = uri.getPath,
        query: String = uri.getQuery,
        fragment: String = uri.getFragment
    ): URI = new URI(scheme, userInfo, host, port, path, query, fragment)

    def update(updatedComponents: UriComponent): URI = this.copy(
      scheme = sOrNull(updatedComponents.scheme.getOrElse(uri.getScheme)),
      userInfo = sOrNull(updatedComponents.userInfo.getOrElse(uri.getUserInfo)),
      host = sOrNull(updatedComponents.host.getOrElse(uri.getHost)),
      port = updatedComponents.port.getOrElse(uri.getPort),
      path = sOrNull((sOrEmpty(uri.getPath) :: updatedComponents.path).mkString("/")),
      query = sOrNull(
        (queryToMap(uri.getQuery) ++ updatedComponents.query)
          .map { case (k, v) => s"$k=$v" }
          .mkString("&")
      ),
      fragment = sOrNull(updatedComponents.fragment.getOrElse(uri.getFragment))
    )
  }

  private def sOrEmpty(string: String) = Option(string).getOrElse("")

  private def sOrNull(string: String): String = if (null == string || string.isEmpty) null else string

  private def queryToMap(query: String): Map[String, String] = {
    for {
      piece <- sOrEmpty(query).split("&")
      if piece.nonEmpty
      Array(k, v) = piece.split("=")
    } yield (k, v)
  }.toMap

  sealed trait Body

  case class StreamBody(s: Any) extends Body

  case class NormalBody(v: Any, codec: CodecForOptional[Any, MediaType, Any]) extends Body

  case class UriComponent(
      scheme: Option[String] = None,
      userInfo: Option[String] = None,
      host: Option[String] = None,
      port: Option[Int] = None,
      path: List[String] = List.empty,
      query: Map[String, String] = Map.empty,
      fragment: Option[String] = None
  )

  case class RequestMetadata(
      cookies: Map[String, String] = Map.empty,
      body: Option[Body] = None,
      header: Map[String, String] = Map.empty,
      method: String = "GET"
  )

  private def paramsTupleToParams[I](params: I): Vector[Any] = ParamsToSeq(params).toVector

  def extractUri[I, E, O, S](baseUri: URI, endpoint: Endpoint[I, E, O, S], params: I): (URI, RequestMetadata) = {
    def setInputParams(
        inputs: Vector[EndpointInput.Single[_]],
        params: Vector[Any],
        paramIndex: Int,
        uri: URI,
        req: RequestMetadata
    ): (URI, RequestMetadata) = {
      def handleMapped[II, T](
          wrapped: EndpointInput[II],
          g: T => II,
          tail: Vector[EndpointInput.Single[_]]
      ): (URI, RequestMetadata) = {
        val (uriParts2, req2) = setInputParams(
          wrapped.asVectorOfSingleInputs,
          paramsTupleToParams(g(params(paramIndex).asInstanceOf[T])),
          0,
          uri,
          req
        )

        setInputParams(tail, params, paramIndex + 1, uriParts2, req2)
      }

      inputs match {
        case Vector() => (uri, req)
        case EndpointInput.FixedMethod(_) +: tail =>
          setInputParams(tail, params, paramIndex, uri, req)
        case EndpointInput.FixedPath(p) +: tail =>
          setInputParams(tail, params, paramIndex, uri.update(UriComponent(path = List(p))), req)
        case EndpointInput.PathCapture(codec, _, _) +: tail =>
          val v = codec.asInstanceOf[PlainCodec[Any]].encode(params(paramIndex): Any)
          setInputParams(tail, params, paramIndex + 1, uri.update(UriComponent(path = List(v))), req)
        case EndpointInput.PathsCapture(_) +: tail =>
          val ps = params(paramIndex).asInstanceOf[Seq[String]]
          setInputParams(tail, params, paramIndex + 1, uri.update(UriComponent(path = ps.toList)), req)
        case EndpointInput.Query(name, codec, _) +: tail =>
          val query: Map[String, String] = codec
            .encode(params(paramIndex))
            .map(v => name -> v)
            .toMap
          val uri2 = uri.update(UriComponent(query = query))
          setInputParams(tail, params, paramIndex + 1, uri2, req)
        case EndpointInput.Cookie(name, codec, _) +: tail =>
          val req2 = codec
            .encode(params(paramIndex))
            .foldLeft(req) { case (r, v) => r.copy(cookies = r.cookies + (name -> v)) }
          setInputParams(tail, params, paramIndex + 1, uri, req2)
        case EndpointInput.QueryParams(_) +: tail =>
          val mqp = params(paramIndex).asInstanceOf[MultiQueryParams]
          val uri2 = uri.update(UriComponent(query = mqp.toMap))
          setInputParams(tail, params, paramIndex + 1, uri2, req)
        case EndpointIO.Body(codec: CodecForOptional[Any, MediaType, Any], _) +: tail =>
          val req2 = req.copy(body = Some(NormalBody(params(paramIndex), codec)))
          setInputParams(tail, params, paramIndex + 1, uri, req2)
        case EndpointIO.StreamBodyWrapper(_) +: tail =>
          val req2 = req.copy(body = Some(StreamBody(params(paramIndex))))
          setInputParams(tail, params, paramIndex + 1, uri, req2)
        case EndpointIO.Header(name, codec, _) +: tail =>
          val headers = codec
            .encode(params(paramIndex))
            .map(name -> _)
            .toMap
          val req2 = req.copy(header = headers)
          setInputParams(tail, params, paramIndex + 1, uri, req2)
        case EndpointIO.Headers(_) +: tail =>
          val headers = params(paramIndex).asInstanceOf[Seq[(String, String)]]
          val req2 = req.copy(header = headers.toMap)
          setInputParams(tail, params, paramIndex + 1, uri, req2)
        case EndpointIO.FixedHeader(name, value, _) +: tail =>
          val header = Map(name -> value)
          val req2 = req.copy(header = header)
          setInputParams(tail, params, paramIndex, uri, req2)
        case EndpointInput.ExtractFromRequest(_) +: tail =>
          // ignoring
          setInputParams(tail, params, paramIndex + 1, uri, req)
        case (a: EndpointInput.Auth[_]) +: tail =>
          setInputParams(a.input +: tail, params, paramIndex, uri, req)
        case EndpointInput.Mapped(wrapped, _, g) +: tail =>
          handleMapped(wrapped, g, tail)
        case EndpointIO.Mapped(wrapped, _, g) +: tail =>
          handleMapped(wrapped, g, tail)
      }
    }

    setInputParams(
      endpoint.input.asVectorOfSingleInputs,
      paramsTupleToParams(params),
      0,
      baseUri,
      RequestMetadata()
    )
  }
}
