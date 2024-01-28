package com.jpmc.json

sealed trait Json extends Product with Serializable

object Json {
  case object Null extends Json

  final case class String(value: Predef.String) extends Json

  final case class Int(value: scala.Int) extends Json

  final case class Double(value: scala.Double) extends Json

  final case class Bool(value: scala.Boolean) extends Json

  sealed trait Doc extends Json

  final case class Array(value: List[Json]) extends Doc

  final case class Obj(value: Map[Predef.String, Json]) extends Doc

  // overloaded apply for each of the types
  def apply(value: Predef.String): Json.String = String(value)

  def apply(value: scala.Int): Json.Int = Int(value)

  def apply(value: scala.Double): Json.Double = Double(value)

  def apply(value: Boolean): Json.Bool = Bool(value)

  def apply(value: List[Json]): Json.Array = Array(value)

  def apply(value: Map[Predef.String, Json]): Json.Doc = Obj(value)

  def removeNull(doc: Doc): Json = doc match {
    case Array(_) => doc
    case Obj(objMap) =>
      Json(
        objMap
          .filter { case (_, value) =>
            value != Null
          }
          .map {
            case (key, value: Doc) => key -> removeNull(value)
            case (key, value)      => key -> value
          }
      )
  }

  def prettyPrint(json: Json): java.lang.String = {
    json match {
      case Null          => "null"
      case String(value) => s""""$value""""
      case Int(value)    => s"$value"
      case Double(value) => s"$value"
      case Bool(value)   => s"$value"
      case Array(values) => s"[${values.map(prettyPrint).mkString(", ")}]"
      case Obj(jsonMap) =>
        s"{${jsonMap
            .map { case (key, value) => prettyPrintKeyValue(key, value) }
            .mkString(",")}}"
    }
  }

  def prettyPrintKeyValue(
    key: java.lang.String,
    value: Json
  ): java.lang.String = s""""$key": ${prettyPrint(value)}"""

  val skipNullPrettyPrint: Doc => Predef.String = removeNull _ andThen prettyPrint
}
