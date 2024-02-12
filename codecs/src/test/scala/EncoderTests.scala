package com.jpmc.codecs

import com.jpmc.codecs.Encoder.User
import com.jpmc.json.Json
import org.scalatest.freespec.AnyFreeSpec

import java.time.LocalDate

class EncoderTests extends AnyFreeSpec {

  "Boolean Encoder" in {
    val booleanTrue = true
    val result = Encoder.encode(booleanTrue)
    val expected = Json.Boolean(true)
    assert(result == expected)
  }

  "String Encoder" in {
    val stringExample = "John Doe"
    val result = Encoder.encode(stringExample)
    val expected = Json.String("John Doe")
    assert(result == expected)
  }

  "Integer Encoder" in {
    val numberExample = 10
    val result = Encoder.encode(numberExample)
    val expected = Json.Number(10)
    assert(result == expected)
  }

  "Date Encoder" in {
    val dateExample = LocalDate.of(2012, 10, 1)
    val result = Encoder.encode(dateExample)
    val expected = Json.String("2012-10-01")
    assert(result == expected)
  }

    "Tuple2 Encoder" in {
      val example: Tuple2[String, Int] = ("John Doe", 100)
      val result = Encoder.encode(example)
      val expected =
        Json.Object(
          Map(
            "1" -> Json.String("John Doe"),
            "2" -> Json.Number(100)
          )
        )
      assert(result == expected)
    }

    "Tuple3 Encoder" in {
      val example: Tuple3[String, Int, Boolean] = ("John Doe", 100, false)
      val result = Encoder.encode(example)
      val expected =
        Json.Object(
          Map(
            "1" -> Json.String("John Doe"),
            "2" -> Json.Number(100),
            "3" -> Json.Boolean(false)
          )
        )
      assert(result == expected)
    }

  "List Encoder" in {
    val example = List("A", "B", "C", "D")
    val result = Encoder.encode(example)
    val expected = Json.Array(List(Json.String("A"), Json.String("B"), Json.String("C"),Json.String("D")))
    assert(result == expected)
  }

  "Vector Encoder" in {
    val example = Vector("A", "B", "C", "D")
    val result = Encoder.encode(example)
    val expected = Json.Array(List(Json.String("A"), Json.String("B"), Json.String("C"), Json.String("D")))
    assert(result == expected)
  }

  "Option Encoder - Some" in {
    val example: Option[Int] = Some(10)
    val result = Encoder.encode(example)
    val expected = Json.Number(10)
    assert(result == expected)
  }

  "Option Encoder - None" in {
    val example: Option[Int] = None
    val result = Encoder.encode(example)
    val expected = Json.Null
    assert(result == expected)
  }

  "Either Encoder Left case - Int, Int" in {
    val example: Either[Int, Int] = Left(4)
    val result = Encoder.encode(example)
    val expected = Json.Object(Map("left" -> Json.Number(4)))
    assert(result == expected)
  }

  "Either Encoder Right case - Int, Int" in {
    val example: Either[Int, Int] = Right(4)
    val result = Encoder.encode(example)
    val expected = Json.Object(Map("right" -> Json.Number(4)))
    assert(result == expected)
  }

  "User encoded correctly" in {
    val example = User("Monica", 24, User.Kind.Guest)
    val result = Encoder.encode(example)
    val expected = Json.Object(Map("name" -> Json.String("Monica"), "age" -> Json.Number(24), "kind" -> Json.String("Guest")))
    assert(result == expected)
  }
}