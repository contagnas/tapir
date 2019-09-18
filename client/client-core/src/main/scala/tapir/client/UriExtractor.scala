package tapir.client

import java.net.URI

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
}
