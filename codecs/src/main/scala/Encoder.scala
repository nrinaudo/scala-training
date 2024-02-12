package com.jpmc.codecs

import com.jpmc.json._

import java.time.LocalDate

trait Encoder[A] {
  def encode(value: A): Json
}

object Encoder {
  def apply[A](implicit encoder: Encoder[A]): Encoder[A] = encoder

  def from[A](f: A => Json): Encoder[A] = (a: A) => f(a)

  implicit class EncoderOps[A](a: A) {
    def asJson(implicit encoder: Encoder[A]): Json = encoder.encode(a)
  }
  def encode[A](value: A)(implicit encoder: Encoder[A]): Json = {
    encoder.encode(value)
  }

  implicit val stringEncoder: Encoder[java.lang.String] = from(Json.String)
  implicit val intEncoder: Encoder[scala.Int] = from(Json.Number)
  implicit val boolEncoder: Encoder[scala.Boolean] = from(bool => Json.Boolean(bool))
  implicit val dateEncoder: Encoder[LocalDate] = from(date => Json.String(date.toString))

  implicit def optionEncoder[A: Encoder]: Encoder[Option[A]] = {
    case Some(value) => value.asJson
    case None => Json.Null
  }

  implicit def listEncoder[A: Encoder]: Encoder[List[A]] = from { data =>
    Json.Array(data.map(_.asJson))
  }

  implicit def vectorEncoder[A: Encoder]: Encoder[Vector[A]] = from { data =>
    Json.Array(data.map(_.asJson).toList)
  }

  implicit def tuple2Encoder[A: Encoder, B: Encoder]: Encoder[Tuple2[A, B]] = from {
    tup =>
      Json.Object(
        Map("1" -> tup._1.asJson,
            "2" -> tup._2.asJson
        ))
  }

  implicit def tuple3Encoder[A: Encoder, B: Encoder, C: Encoder]: Encoder[Tuple3[A, B, C]] = from {
    tup =>
      Json.Object(
        Map("1" -> tup._1.asJson,
            "2" -> tup._2.asJson,
            "3" -> tup._3.asJson
        ))
  }

  implicit def eitherEncoder[A: Encoder, B: Encoder]: Encoder[Either[A, B]] = {
    case Right(value) => Json.Object(Map("right" -> value.asJson))
    case Left(value)  => Json.Object(Map("left" -> value.asJson))
  }

  implicit val kindEncoder: Encoder[User.Kind] = {
    case User.Kind.Privileged => Json.String("Privileged")
    case User.Kind.Normal => Json.String("Normal")
    case User.Kind.Guest => Json.String("Guest")
  }

  implicit val userEncoder: Encoder[User] = from { user => {
    Json.Object(Map("name" -> user.name.asJson, "age" -> user.age.asJson, "kind" -> user.kind.asJson))
  }}

  case class User(name: java.lang.String, age: Int, kind: User.Kind)

  object User {
    sealed trait Kind
    object Kind {
      case object Privileged extends Kind
      case object Normal extends Kind
      case object Guest extends Kind
    }
  }
}

