# tapir, or Typed API descRiptions

With tapir you can describe HTTP API endpoints as immutable Scala values. Each endpoint can contain a number of 
input parameters, error-output parameters, and normal-output parameters. An endpoint specification can be 
interpreted as:

* a server, given the "business logic": a function, which computes output parameters based on input parameters. 
  Currently supported: 
  * [Akka HTTP](server/akkahttp.html) `Route`s/`Directive`s.
  * [Http4s](server/http4s.html) `HttpRoutes[F]`
* a client, which is a function from input parameters to output parameters. Currently supported: [sttp](sttp.html).
* documentation. Currently supported: [OpenAPI](openapi.html).

Tapir is licensed under Apache2, the source code is [available of GitHub](https://github.com/softwaremill/tapir).

Depending on how you prefer to explore the library, take a look at one of the [examples](examples.md) or read on
for a more detailed description of how tapir works!

## Code teaser

```scala
import tapir._
import tapir.json.circe._
import io.circe.generic.auto._

type Limit = Int
type AuthToken = String
case class BooksFromYear(genre: String, year: Int)
case class Book(title: String)

val booksListing: Endpoint[(BooksFromYear, Limit, AuthToken), String, List[Book], Nothing] = 
  endpoint
    .get
    .in(("books" / path[String]("genre") / path[Int]("year")).mapTo(BooksFromYear))
    .in(query[Limit]("limit").description("Maximum number of books to retrieve"))
    .in(header[AuthToken]("X-Auth-Token"))
    .errorOut(stringBody)
    .out(jsonBody[List[Book]])

//

import tapir.docs.openapi._
import tapir.openapi.circe.yaml._

val docs = booksListing.toOpenAPI("My Bookshop", "1.0")
println(docs.toYaml)

//

import tapir.server.akkahttp._
import akka.http.scaladsl.server.Route
import scala.concurrent.Future

def bookListingLogic(bfy: BooksFromYear, 
                     limit: Limit,  
                     at: AuthToken): Future[Either[String, List[Book]]] =
  Future.successful(Right(List(Book("The Sorrows of Young Werther"))))
val booksListingRoute: Route = booksListing.toRoute(bookListingLogic _)

//

import tapir.client.sttp._
import com.softwaremill.sttp._

val booksListingRequest: Request[Either[String, List[Book]], Nothing] = booksListing
  .toSttpRequest(uri"http://localhost:8080")
  .apply(BooksFromYear("SF", 2016), 20, "xyz-abc-123")
```

## Contents

* [Quickstart](quickstart.md)
* [Examples](examples.md)
* [Goals of the project](goals.md)
* [Endpoints: basics](endpoint/basics.md)
* [Endpoints: inputs/outputs](endpoint/ios.md)
* [Endpoints: codecs](endpoint/codecs.md)
* [Endpoints: custom types](endpoint/customtypes.md)
* [Endpoints: validation](endpoint/validation.md)
* [Endpoints: working with JSON](endpoint/json.md)
* [Endpoints: forms](endpoint/forms.md)
* [Endpoints: authentication](endpoint/auth.md)
* [Servers: akka-http interpreter](server/akkahttp.md)
* [Servers: http4s interpreter](server/http4s.md)
* [Servers: options](server/options.md)
* [Servers: logic](server/logic.md)
* [Servers: error handling](server/errors.md)
* [Servers: debugging](server/debugging.md)
* [Clients: sttp interpreter](sttp.md)
* [Other interpreters](other_interpreters.md)
* [Documentation: openapi interpreter](openapi.md)
* [Create your own tapir](mytapir.md)
* [Design notes](design.md)
* [Contributing](contributing.md)
