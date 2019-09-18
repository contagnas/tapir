package tapir.client


import java.net.URI

import tapir.Codec.PlainCodec
import tapir.{ByteArrayValueType, ByteBufferValueType, CodecForOptional, CodecMeta, Endpoint, EndpointIO, EndpointInput, FileValueType, InputStreamValueType, MediaType, MultipartValueType, RawPart, StringValueType}
import tapir.internal._
import tapir.model.{MultiQueryParams, Part}

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
      query = sOrNull((queryToMap(uri.getQuery) ++ updatedComponents.query)
        .map { case (k, v) => s"$k=$v" }.mkString("&")),
      fragment = sOrNull(updatedComponents.fragment.getOrElse(uri.getFragment)),
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
    cookies: Map[String, String] = Map.empty
  )

  private def paramsTupleToParams[I](params: I): Vector[Any] = ParamsToSeq(params).toVector

  private def setBody[T, M <: MediaType, R](v: T, codec: CodecForOptional[T, M, R], req: PartialAnyRequest): PartialAnyRequest = {
    codec
      .encode(v)
      .map { t =>
        val req2 = codec.meta.rawValueType match {
          case StringValueType(charset) => req.body(t, charset.name())
          case ByteArrayValueType       => req.body(t)
          case ByteBufferValueType      => req.body(t)
          case InputStreamValueType     => req.body(t)
          case FileValueType            => req.body(t)
          case mvt: MultipartValueType =>
            val parts: Seq[Multipart] = (t: Seq[RawPart]).flatMap { p =>
              mvt.partCodecMeta(p.name).map { partCodecMeta =>
                val sttpPart1 = partToSttpPart(p.asInstanceOf[Part[Any]], partCodecMeta.asInstanceOf[CodecMeta[_, _, Any]])
                val sttpPart2 = sttpPart1.contentType(partCodecMeta.mediaType.mediaTypeNoParams)
                val sttpPart3 = p.headers.foldLeft(sttpPart2) {
                  case (sp, (hk, hv)) =>
                    if (hk.equalsIgnoreCase(HeaderNames.ContentType)) {
                      sp.contentType(hv)
                    } else {
                      sp.header(hk, hv)
                    }
                }
                p.fileName.map(sttpPart3.fileName).getOrElse(sttpPart3)
              }
            }

            req.multipartBody(parts.toList)
        }

        req2.header(HeaderNames.ContentType, codec.meta.mediaType.mediaType, replaceExisting = false)
      }
      .getOrElse(req)
  }

  def extractUri[I, E, O, S](baseUri: URI, endpoint: Endpoint[I, E, O, S], params: I): URI = {

    private def setInputParams[I](
      inputs: Vector[EndpointInput.Single[_]],
      params: Vector[Any],
      paramIndex: Int,
      uri: Uri,
      req: PartialAnyRequest
    ): (Uri, PartialAnyRequest)

    =
    {
      def handleMapped[II, T](
        wrapped: EndpointInput[II],
        g: T => II,
        tail: Vector[EndpointInput.Single[_]]
      ): (Uri, PartialAnyRequest) = {
        val (uri2, req2) = setInputParams(
          wrapped.asVectorOfSingleInputs,
          paramsTupleToParams(g(params(paramIndex).asInstanceOf[T])),
          0,
          uri,
          req
        )

        setInputParams(tail, params, paramIndex + 1, uri2, req2)
      }

      inputs match {
        case Vector() => (uri, req)
        case EndpointInput.FixedMethod(_) +: tail =>
          setInputParams(tail, params, paramIndex, uri, req)
        case EndpointInput.FixedPath(p) +: tail =>
          setInputParams(tail, params, paramIndex, uri.copy(path = uri.path :+ p), req)
        case EndpointInput.PathCapture(codec, _, _) +: tail =>
          val v = codec.asInstanceOf[PlainCodec[Any]].encode(params(paramIndex): Any)
          setInputParams(tail, params, paramIndex + 1, uri.copy(path = uri.path :+ v), req)
        case EndpointInput.PathsCapture(_) +: tail =>
          val ps = params(paramIndex).asInstanceOf[Seq[String]]
          setInputParams(tail, params, paramIndex + 1, uri.copy(path = uri.path ++ ps), req)
        case EndpointInput.Query(name, codec, _) +: tail =>
          val uri2 = codec
            .encode(params(paramIndex))
            .foldLeft(uri) { case (u, v) => u.param(name, v) }
          setInputParams(tail, params, paramIndex + 1, uri2, req)
        case EndpointInput.Cookie(name, codec, _) +: tail =>
          val req2 = codec
            .encode(params(paramIndex))
            .foldLeft(req) { case (r, v) => r.cookie(name, v) }
          setInputParams(tail, params, paramIndex + 1, uri, req2)
        case EndpointInput.QueryParams(_) +: tail =>
          val mqp = params(paramIndex).asInstanceOf[MultiQueryParams]
          val uri2 = uri.params(mqp.toSeq: _*)
          setInputParams(tail, params, paramIndex + 1, uri2, req)
        case EndpointIO.Body(codec, _) +: tail =>
          val req2 = setBody(params(paramIndex), codec, req)
          setInputParams(tail, params, paramIndex + 1, uri, req2)
        case EndpointIO.StreamBodyWrapper(_) +: tail =>
          val req2 = req.streamBody(params(paramIndex))
          setInputParams(tail, params, paramIndex + 1, uri, req2)
        case EndpointIO.Header(name, codec, _) +: tail =>
          val req2 = codec
            .encode(params(paramIndex))
            .foldLeft(req) { case (r, v) => r.header(name, v) }
          setInputParams(tail, params, paramIndex + 1, uri, req2)
        case EndpointIO.Headers(_) +: tail =>
          val headers = params(paramIndex).asInstanceOf[Seq[(String, String)]]
          val req2 = headers.foldLeft(req) {
            case (r, (k, v)) =>
              val replaceExisting = HeaderNames.ContentType.equalsIgnoreCase(k) || HeaderNames.ContentLength.equalsIgnoreCase(k)
              r.header(k, v, replaceExisting)
          }
          setInputParams(tail, params, paramIndex + 1, uri, req2)
        case EndpointIO.FixedHeader(name, value, _) +: tail =>
          val req2 = Seq(value)
            .foldLeft(req) { case (r, v) => r.header(name, v) }
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
  }
}
