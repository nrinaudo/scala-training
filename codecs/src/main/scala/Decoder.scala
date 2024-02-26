package com.jpmc.codecs

import com.jpmc.codecs.Decoder.DecoderResult
import com.jpmc.codecs.Encoder.User
import com.jpmc.json.Json

import java.time.LocalDate
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
case class DecoderErr(message: String)
trait Decoder[A] {
  def map[B](f: A => B): Decoder[B] = Decoder.from { j =>
    this.decode(j).map(f)
  }
  def decode(value: Json): DecoderResult[A]
  // Json to an Either[String, A]
  def withErr(message: String): Decoder[A] = Decoder.from { json =>
    decode(json).left.map(_ => DecoderErr(message))
  }
}

object Decoder {
  type DecoderResult[A] = Either[DecoderErr, A]
  def apply[A](implicit decoder: Decoder[A]): Decoder[A] = decoder

  def from[A](f: Json => DecoderResult[A]): Decoder[A] = (j: Json) => f(j)

  private def fromPartial[A](pf: PartialFunction[Json, DecoderResult[A]]): Decoder[A] = (j: Json) =>
    pf.lift.apply(j) match {
      case Some(json) => json
      case None => Left(DecoderErr("No decoder found, match error."))
    }

  implicit class DecoderOps(value: Json) {
    def as[A](implicit decoder: Decoder[A]): DecoderResult[A] = decoder.decode(value)
  }
  implicit class DocDecoderOps(jsonObj: Json.Object) {
    def decodeMap[A](key: String)(implicit decoder: Decoder[A]): DecoderResult[A] =
      jsonObj.data.get(key)
        .toRight(DecoderErr(s"Unable to find value at key = ${key}"))
        .flatMap(_.as[A])
  }

  implicit val stringDecoder: Decoder[String] = fromPartial {
    case Json.String(str) => Right(str)
  }.withErr("Could not decode String")

  implicit val intDecoder: Decoder[Int] = fromPartial {
    case Json.Number(num) => Right(num)
  }.withErr("Could not decode Int")

  implicit val boolDecoder: Decoder[java.lang.Boolean] = fromPartial {
    case Json.Boolean(bool) => Right(bool)
  }.withErr("Could not decode Boolean")

  implicit val dateDecoder: Decoder[LocalDate] = fromPartial {
    case Json.String(str) => Try(LocalDate.parse(str)) match {
      case Failure(exp) => Left(DecoderErr(s"Failure to parse Local date time: ${str}, with exception ${exp.getMessage}"))
      case Success(value) => Right(value)
    }
  }

  implicit def tuple2Decoder[A: Decoder, B: Decoder]: Decoder[Tuple2[A, B]] = fromPartial {
    case obj: Json.Object => for {
      val1 <- obj.decodeMap[A]("1")
      val2 <- obj.decodeMap[B]("2")
    } yield (val1, val2)
  }.withErr("Could not decode Tuple 2")

  implicit def tuple3Decoder[A: Decoder, B: Decoder, C: Decoder]: Decoder[Tuple3[A, B, C]] = fromPartial {
    case obj: Json.Object => for {
      val1 <- obj.decodeMap[A]("1")
      val2 <- obj.decodeMap[B]("2")
      val3 <- obj.decodeMap[C]("3")
    } yield (val1, val2, val3)
  }.withErr("Could not decode Tuple 3")

  implicit def eitherDecoder[A: Decoder, B: Decoder]: Decoder[Either[A, B]] = fromPartial {
    case Json.Object(m) if m.contains("left") =>
      m.get("left").toRight(DecoderErr("Failed to get Left value")).flatMap(_.as[A]).map(Left(_))
    case Json.Object(m) if m.contains("right") =>
      m.get("right").toRight(DecoderErr("Failed to get Right value")).flatMap(_.as[B]).map(Right(_))
  }

  implicit def optionDecoder[A: Decoder]: Decoder[Option[A]] = fromPartial {
    case Json.Null => Right(Option.empty[A])
    case j: Json =>
      j.as[A].map(Option.apply)
  }.withErr(s"Could not decode Option")

  // Going from F[G[A]] to G[F[A]]
  // F = List
  // G = DecoderResult
  implicit def listDecoder[A: Decoder]: Decoder[List[A]] = fromPartial {
    case Json.Array(data) =>
      @tailrec
      def loop(resList: List[DecoderResult[A]], acc: DecoderResult[List[A]]): DecoderResult[List[A]] = {
        resList match {
          case Nil => acc
          case head :: tail =>
            val accumulating = for {
              al <- acc
              a <- head
            } yield a :: al
            loop(tail, accumulating)
        }
      }
      val resList = data.map(json => json.as[A]) // give me a list of Decoded Results
      val initialList = List.empty[A]
      loop(resList, Right(initialList).map(_.reverse))
  }.withErr("Could not decode Json Array to List")

  implicit def vectorDecoder[A: Decoder]: Decoder[Vector[A]] = listDecoder[A].withErr("Could not decode Json Array to Vector").map(_.toVector)

  implicit val userDecoder: Decoder[User] = fromPartial {
    case o: Json.Object => for {
      name <- o.decodeMap[String]("name")
      age <- o.decodeMap[Int]("age")
      kind <- o.decodeMap[User.Kind]("kind")
    } yield User(name, age, kind)
  }.withErr("Could not decode Json Object to User")

  implicit val kindDecoder: Decoder[User.Kind] = fromPartial {
    case Json.String(str) =>
      str match {
        case "Privileged" => Right(User.Kind.Privileged)
        case "Normal" => Right(User.Kind.Normal)
        case "Guest" => Right(User.Kind.Guest)
        case _ => Left(DecoderErr("Unable to decode to Kind"))
      }
  }
}