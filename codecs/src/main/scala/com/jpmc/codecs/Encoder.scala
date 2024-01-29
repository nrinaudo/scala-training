package com.jpmc.codecs

import com.jpmc.json._

import java.time.LocalDate
import java.time.format.DateTimeFormatter

trait Encoder[A] {
  def encode(value: A): Json

  def contraMap[B](f: B => A): Encoder[B] = Encoder.from { b => this.encode(f(b)) }
}

object Encoder {
  def apply[A](implicit encoder: Encoder[A]): Encoder[A] = encoder

  def from[A](f: A => Json): Encoder[A] = (value: A) => f(value)

  implicit class EncoderSyntax[A](value: A) {
    def asJson(implicit encoder: Encoder[A]): Json = encoder.encode(value)
  }

  implicit val intEncoder: Encoder[Int] = Encoder.from(intVal => Json(intVal))
  implicit val stringEncoder: Encoder[String] = Encoder.from(strVal => Json(strVal))
  implicit val booleanEncoder: Encoder[Boolean] = Encoder.from(boolVal => Json(boolVal))
  implicit val dateEncoder: Encoder[LocalDate] = Encoder.from(dateVal => Json(DateTimeFormatter.ISO_LOCAL_DATE.format(dateVal)))

  implicit def listEncoder[A: Encoder]: Encoder[List[A]] = from(elements => Json(elements.map(_.asJson)))
  implicit def vectorEncoder[A: Encoder]: Encoder[Vector[A]] = from(elements => Json(elements.toList.map(_.asJson)))

  implicit def tuple2Encoder[A: Encoder, B: Encoder]: Encoder[(A, B)] = from {
    case (a, b) => Json(List(a.asJson, b.asJson))
  }

  implicit def tuple3Encoder[A: Encoder, B: Encoder, C: Encoder]: Encoder[(A, B, C)] = from {
    case (a, b, c) => Json(List(a.asJson, b.asJson, c.asJson))
  }

  implicit def optionEncoder[A](implicit encoder: Encoder[A]): Encoder[Option[A]] = from {
    case Some(value) => value.asJson
    case None        => Json.Null
  }

  implicit def eitherEncoder[A, B](implicit leftEncoder: Encoder[A], rightEncoder: Encoder[B]): Encoder[Either[A, B]] = from {
    case Left(value) => Json(Map("side"-> "left".asJson, "value" -> value.asJson))
    case Right(value) => Json(Map("side"-> "right".asJson, "value" -> value.asJson))
  }

  case class User(name: String, age: Int, kind: User.Kind)

  object User {
    sealed trait Kind

    object Kind {
      case object Privileged extends Kind

      case object Normal extends Kind

      case object Guest extends Kind
    }
  }

  implicit val userKindEncoder: Encoder[User.Kind] = Encoder[String].contraMap(_.toString)

  implicit val userEncoder: Encoder[User] = from { user =>
    Json(Map(
      "name" -> user.name.asJson,
      "age"  -> user.age.asJson,
      "kind" -> user.kind.asJson
    ))
  }
}

