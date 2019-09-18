package tapir.client.http4s

case class Http4sClientOptions()

object Http4sClientOptions {
  implicit val default: Http4sClientOptions = Http4sClientOptions()
}
