package com.jpmc.codecs

import com.jpmc.json.Json
import Decoder.DecodeResult
import com.jpmc.codecs.Encoder.User

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class DecodeError(message: String) {
  def combine(other: DecodeError): DecodeError =
    DecodeError(s"${this.message}, ${other.message}")
}


trait Decoder[A] {
  def decode(value: Json): DecodeResult[A]

  def withError(message: String): Decoder[A] = Decoder.from { jsonVal =>
    decode(jsonVal).left.map(originalError => originalError combine DecodeError(message))
  }

  def map[B](f: A => B): Decoder[B] = Decoder.from { jsonVal =>
    this.decode(jsonVal).map(f)
  }
}

object Decoder {
  type DecodeResult[A] = Either[DecodeError, A]

  def apply[A](implicit decoder: Decoder[A]): Decoder[A] = decoder

  def from[A](f: Json => DecodeResult[A]): Decoder[A] = (value: Json) => f(value)

  def fromPartial[A](f: PartialFunction[Json, DecodeResult[A]]): Decoder[A] = (value: Json) =>
    f.lift.apply(value) match {
      case Some(value) => value
      case None => Left(DecodeError("decode match error"))
    }

  implicit class DecoderSyntax(value: Json) {
    def as[A](implicit decoder: Decoder[A]): DecodeResult[A] = decoder.decode(value)
  }

  implicit class JsonDocGetDecode(doc: Json.Obj) {
    def getDecode[A](key: String)(implicit decoder: Decoder[A]): DecodeResult[A] =
      doc.value.get(key).toRight(DecodeError(s"Failed to find $key")).flatMap(_.as[A])
  }

  implicit val intDecoder: Decoder[Int] = fromPartial {
    case Json.Int(intVal) => Right(intVal)
  }.withError("int decode error")

  implicit val stringDecoder: Decoder[String] = fromPartial {
    case Json.String(strVal) => Right(strVal)
  }.withError("string decode error")

  implicit val booleanDecoder: Decoder[Boolean] = fromPartial {
    case Json.Bool(strVal) => Right(strVal)
  }.withError("bool decode error")

  implicit val dateDecoder: Decoder[LocalDate] = fromPartial {
    case Json.String(strVal) => Try(LocalDate.parse(strVal, DateTimeFormatter.ISO_LOCAL_DATE)) match {
      case Failure(exception) => Left(DecodeError(exception.getMessage))
      case Success(value) => Right(value)
    }
  }.withError("date decode error")

  implicit def listDecoder[A: Decoder]: Decoder[List[A]] = fromPartial {
    case Json.Array(values) =>
      @tailrec
      def loop(rem: List[DecodeResult[A]], acc: DecodeResult[List[A]]): DecodeResult[List[A]] = {
        rem match {
          case Nil => acc
          case head :: tail =>
            val accWithHead = for {
              accList <- acc
              a <- head
            } yield a :: accList

            loop(tail, accWithHead)
        }
      }

      loop(values.map(_.as[A]), Right(List.empty[A]))
        .map(_.reverse)
  }.withError("list decode error")

  implicit def vectorDecoder[A: Decoder]: Decoder[Vector[A]] = listDecoder[A].map(_.toVector)

  implicit def tuple2Decoder[A: Decoder, B: Decoder]: Decoder[(A, B)] = fromPartial {
    case Json.Array(List(a, b)) => for {
      aRes <- a.as[A]
      bRes <- b.as[B]
    } yield (aRes, bRes)
  }.withError("tuple2 decode error")

  implicit def tuple3Decoder[A: Decoder, B: Decoder, C: Decoder]: Decoder[(A, B, C)] = fromPartial {
    case Json.Array(List(a, b, c)) => for {
      aRes <- a.as[A]
      bRes <- b.as[B]
      cRes <- c.as[C]
    } yield (aRes, bRes, cRes)
  }.withError("tuple3 decode error")

  implicit def optionDecoder[A: Decoder]: Decoder[Option[A]] = fromPartial {
    case Json.Null => Right(Option.empty[A])
    case v: Json   => v.as[A].map(Some(_))
  }

  implicit def eitherDecoder[A: Decoder, B: Decoder]: Decoder[Either[A, B]] = fromPartial {
    case Json.Obj(objMap) =>
      objMap.get("side")
        .toRight(DecodeError("either decode error"))
        .flatMap {
          case Json.String(value) if value == "left"  => objMap.get("value").toRight(DecodeError("either decode error"))
            .flatMap(_.as[A]).map(Left(_))
          case Json.String(value) if value == "right" => objMap.get("value").toRight(DecodeError("either decode error"))
            .flatMap(_.as[B]).map(Right(_))
      }
  }

  implicit val kindDecoder: Decoder[User.Kind] = fromPartial {
    case Json.String(strVal) => strVal match {
      case "Privileged" => Right(User.Kind.Privileged)
      case "Normal"     => Right(User.Kind.Normal)
      case "Guest"      => Right(User.Kind.Guest)
      case _            => Left[DecodeError, User.Kind](DecodeError("kind decode error"))
    }
  }.withError("kind decode error")

  implicit val userDecoder: Decoder[User] = fromPartial {
    case value: Json.Obj => for {
      name    <- value.getDecode[String]("name")
      age     <- value.getDecode[Int]("age")
      kind    <- value.getDecode[User.Kind]("kind")
    } yield User(name, age, kind)
  }.withError("user decode error")
}