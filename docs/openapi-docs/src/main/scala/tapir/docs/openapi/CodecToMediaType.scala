package tapir.docs.openapi

import tapir.docs.openapi.schema.{EncodeAny, ObjectSchemas}
import tapir.openapi.{MediaType => OMediaType, _}
import tapir.{MediaType => SMediaType, Schema => SSchema, _}

import scala.collection.immutable.ListMap

private[openapi] class CodecToMediaType(objectSchemas: ObjectSchemas) {
  def apply[T, M <: SMediaType](o: CodecForOptional[T, M, _], example: Option[T]): ListMap[String, OMediaType] = {
    ListMap(
      o.meta.mediaType.mediaTypeNoParams -> OMediaType(
        Some(objectSchemas(o.meta.schema, o.validator, Option({ t: T =>
          encodeValue(o, t)
        }))),
        example.flatMap(exampleValue(o, _)),
        ListMap.empty,
        ListMap.empty
      )
    )
  }

  def apply[M <: SMediaType](
      schema: SSchema,
      mediaType: M,
      example: Option[String]
  ): ListMap[String, OMediaType] = {
    ListMap(
      mediaType.mediaTypeNoParams -> OMediaType(
        Some(objectSchemas(schema, Validator.pass, Option.empty[EncodeAny[_]])),
        example.map(ExampleValue),
        ListMap.empty,
        ListMap.empty
      )
    )
  }
}
