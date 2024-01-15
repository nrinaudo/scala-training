package com.jpmc.json

sealed trait Json extends Product with Serializable

object Json {
  case object JNull extends Json
  final case class JString(value: String) extends Json
  final case class JInt(value: Int) extends Json
  final case class JDouble(value: Double) extends Json
  final case class JBool(value: Boolean) extends Json
  sealed trait JDoc extends Json
  final case class JArray(value: List[Json]) extends JDoc
  final case class JObj(value: Map[String, Json]) extends JDoc

  // overloaded apply for each of the types
  def apply(value: String): Json = JString(value)
  def apply(value: Int): Json = JInt(value)
  def apply(value: Double): Json = JDouble(value)
  def apply(value: Boolean): Json = JBool(value)
  def apply(value: List[Json]): JDoc = JArray(value)
  def apply(value: Map[String, Json]): JDoc = JObj(value)


  def prettyPrint(v: Json): String = {
    v match {
      case JNull => "null"
      case JString(value) => "\"" + value + "\""
      case JInt(value) => s"$value"
      case JDouble(value) => s"$value"
      case JBool(value) => s"$value"
      case z: JDoc => prettyPrintDoc(z)
    }
  }

  def prettyPrintSkipNull(v: Json): Option[String] = {
    v match {
      case JNull => None
      case JString(value) => Some("\"" + value + "\"")
      case JInt(value) => Some(s"$value")
      case JDouble(value) => Some(s"$value")
      case JBool(value) => Some(s"$value")
      case z: JDoc => Some(prettyPrintDocRemoveNulls(z))
    }
  }

  def prettyPrintDoc(v: JDoc): String = {
    v match {
      case JArray(value) => "[" + value.map(prettyPrint).mkString(", ") + "]"
      case JObj(value) => value.toList.map {
        case (key, value) => "\"" + key + "\": " + prettyPrint(value)
      }.mkString("{", ",", "}")
    }
  }

  def prettyPrintDocRemoveNulls(v: JDoc) = {
    v match {
      case JArray(value) => "[" +
        value.filter(x => x != JNull)
          .map(prettyPrintSkipNull)
          .filter(_.isDefined)
          .map(_.get)
          .mkString(", ") +
        "]"
      case JObj(value) => value.toList.filter {
        case (_, value) => value != JNull
      }.map {
        case (key, value) => prettyPrintSkipNull(value).map("\"" + key + "\": " + _)
      }.filter(_.isDefined).map(_.get).mkString("{", ", ", "}")
    }
  }
}