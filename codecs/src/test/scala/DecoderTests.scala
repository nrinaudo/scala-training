package com.jpmc.codecs

import com.jpmc.codecs.Encoder.User
import com.jpmc.json.Json
import org.scalatest.freespec.AnyFreeSpec

import java.time.LocalDate

class DecoderTests extends AnyFreeSpec {
  "String Decoder" - {
    "Successful decoding" in {
      val str = "decode me"
      val jsonStr = Json.String(str)
      val expected = Right(str)
      val result = Decoder.DecoderOps(jsonStr).as[String]
      assert(result == expected)
    }
    "Failure decoding" in {
      val jsonNum = Json.Number(123)
      val expected = Left(DecoderErr("Could not decode String"))
      val result = Decoder.DecoderOps(jsonNum).as[String]
      assert(result == expected)
    }
  }
  "Int Decoder" - {
    "Successful decoding" in {
      val num = 123
      val jsonNum = Json.Number(num)
      val expected = Right(num)
      val result = Decoder.DecoderOps(jsonNum).as[Int]
      assert(result == expected)
    }
    "Failure decoding" in {
      val jsonNum = Json.String("this is going to fail decoding")
      val expected = Left(DecoderErr("Could not decode Int"))
      val result = Decoder.DecoderOps(jsonNum).as[Int]
      assert(result == expected)
    }
  }
  "Bool Decoder" - {
    "Successful decoding" in {
      val bool = java.lang.Boolean.TRUE
      val jsonBool = Json.Boolean(bool)
      val expected = Right(bool)
      val result = Decoder.DecoderOps(jsonBool).as[java.lang.Boolean]
      assert(result == expected)
    }
    "Failure decoding" in {
      val jsonNum = Json.Number(123)
      val expected = Left(DecoderErr("Could not decode Boolean"))
      val result = Decoder.DecoderOps(jsonNum).as[java.lang.Boolean]
      assert(result == expected)
    }
  }
  "LocalDate Decoder" - {
    "Successful decoding" in {
      val dateStr = LocalDate.parse("2012-10-01")
      val date = Json.String("2012-10-01")
      val expected = Right(dateStr)
      val result = Decoder.DecoderOps(date).as[LocalDate]
      assert(result == expected)
    }
    "Successful decoding to Left but failure to parse date" in {
      val date = Json.String("2012-10-43-5")
      val expected = Left(DecoderErr(s"Failure to parse Local date time: 2012-10-43-5, with exception Text '2012-10-43-5' could not be parsed, unparsed text found at index 10"))
      val result = Decoder.DecoderOps(date).as[LocalDate]
      assert(result == expected)
    }
    "Failure decoding" in {
      val jsonNum = Json.Number(123)
      val expected = Left(DecoderErr("No decoder found, match error."))
      val result = Decoder.DecoderOps(jsonNum).as[LocalDate]
      assert(result == expected)
    }
  }
  "Tuple 2 Decoder" - {
    "Successful decoding" in {
      val jsonObj = Json.Object(Map("1" -> Json.String("value 1"), "2" -> Json.String("value 2")))
      val expected = Right(("value 1", "value 2"))
      val result = Decoder[(String, String)].decode(jsonObj)
      assert(result == expected)
    }
    "Failure decoding" in {
      val jsonObj = Json.Object(Map("1" -> Json.String("value 1"), "2" -> Json.Number(123)))
      val expected = Left(DecoderErr("Could not decode Tuple 2"))
      val result = Decoder[(String, String)].decode(jsonObj)
      assert(result == expected)
    }
  }
  "Tuple 3 Decoder" - {
    "Successful decoding" in {
      val jsonObj = Json.Object(Map("1" -> Json.String("value 1"), "2" -> Json.String("value 2"), "3" -> Json.Number(123)))
      val expected = Right(("value 1", "value 2", 123))
      val result = Decoder[(String, String, Int)].decode(jsonObj)
      assert(result == expected)
    }
    "Failure decoding" in {
      val jsonObj = Json.Object(Map("1" -> Json.String("value 1"), "2" -> Json.Number(123)))
      val expected = Left(DecoderErr("Could not decode Tuple 3"))
      val result = Decoder[(String, String, String)].decode(jsonObj)
      assert(result == expected)
    }
  }
  "Either Decoder" - {
    "Left" - {
      "Successful decoding" in {
        val jsonObj = Json.Object(Map("left" -> Json.String("This is a Left")))
        val expected = Right(Left("This is a Left"))
        val result = Decoder[Either[String, String]].decode(jsonObj)
        assert(result == expected)
      }
      "Failure decoding" in {
        val jsonObj = Json.Object(Map("le" -> Json.String("This should not decode")))
        val expected = Left(DecoderErr("No decoder found, match error."))
        val result = Decoder[Either[String, String]].decode(jsonObj)
        assert(result == expected)
      }
    }
    "Right" - {
      "Successful decoding" in {
        val jsonObj = Json.Object(Map("right" -> Json.Number(123)))
        val expected = Right(Right(123))
        val result = Decoder[Either[String, Int]].decode(jsonObj)
        assert(result == expected)
      }
      "Failure decoding" in {
        val jsonObj = Json.Object(Map("re" -> Json.String("This should not decode")))
        val expected = Left(DecoderErr("No decoder found, match error."))
        val result = Decoder[Either[String, String]].decode(jsonObj)
        assert(result == expected)
      }
    }
  }
  "Option Decoder" - {
    "Successful decoding of Some value" in {
      val json = Json.String("Hello Some")
      val expected = Right(Some("Hello Some"))
      val result = Decoder[Option[String]].decode(json)
      assert(result == expected)
    }
    "Successful decoding of None value" in {
      val json = Json.Null
      val expected = Right(Option.empty[String])
      val result = Decoder[Option[String]].decode(json)
      assert(result == expected)
    }
    "Failure decoding" in {
      val json = Json.Number(123)
      val expected = Left(DecoderErr("Could not decode Option"))
      val result = Decoder[Option[String]].decode(json)
      assert(result == expected)
    }
  }
  "List Decoder" - {
    "Successful decoding" in {
      val jsonArr = Json.Array(List(Json.String("entry 1"), Json.String("entry 2")))
      val expected = Right(List("entry 1", "entry 2"))
      val result = Decoder[List[String]].decode(jsonArr)
      assert(result == expected)
    }
    "Failure decoding" in {
      val jsonArr = Json.Array(List(Json.String("entry 1"), Json.String("entry 2"), Json.Number(1)))
      val expected = Left(DecoderErr("Could not decode Json Array to List"))
      val result = Decoder[List[String]].decode(jsonArr)
      assert(result == expected)
    }
  }
  "Vector Decoder" - {
    "Successful decoding" in {
      val jsonArr = Json.Array(List(Json.String("entry 1"), Json.String("entry 2")))
      val expected = Right(Vector("entry 1", "entry 2"))
      val result = Decoder[Vector[String]].decode(jsonArr)
      assert(result == expected)
    }
    "Failure decoding" in {
      val jsonArr = Json.Array(List(Json.String("entry 1"), Json.String("entry 2"), Json.Number(1)))
      val expected = Left(DecoderErr("Could not decode Json Array to Vector"))
      val result = Decoder[Vector[String]].decode(jsonArr)
      assert(result == expected)
    }
  }
  "User Decoder" - {
    "Successful decoding" in {
      val jsonObj = Json.Object(Map("name" -> Json.String("Mon"), "age" -> Json.Number(24), "kind" -> Json.String("Normal")))
      val expected = Right(User("Mon", 24, User.Kind.Normal))
      val result = Decoder[User].decode(jsonObj)
      assert(result == expected)
    }
    "Failure decoding" in {
      val str = Json.String("This is not a user.")
      val expected = Left(DecoderErr("Could not decode Json Object to User"))
      val result = Decoder[User].decode(str)
      assert(result == expected)
    }
  }
  "User Kind Decoder" - {
    "Successful decoding of Privileged" in {
      val str = Json.String("Privileged")
      val expected = Right(User.Kind.Privileged)
      val result = Decoder.DecoderOps(str).as[User.Kind]
      assert(result == expected)
    }
    "Successful decoding of Guest" in {
      val str = Json.String("Guest")
      val expected = Right(User.Kind.Guest)
      val result = Decoder[User.Kind].decode(str)
      assert(result == expected)
    }
    "Successful decoding of Normal" in {
      val str = Json.String("Normal")
      val expected = Right(User.Kind.Normal)
      val result = Decoder[User.Kind].decode(str)
      assert(result == expected)
    }
    "Failure decoding" in {
      val str = Json.String("Other")
      val expected = Left(DecoderErr("Unable to decode to Kind"))
      val result = Decoder[User.Kind].decode(str)
      assert(result == expected)
    }
  }
  "Map Decoder Ops" - {
    import com.jpmc.codecs.Decoder.stringDecoder
    val map = Json.Object(Map("1" -> Json.String("value 1"), "2" -> Json.String("value 2")))
    "Successful decoding" in {
      val expected = Right("value 1")
      val result = Decoder.DocDecoderOps(map).decodeMap("1")
      assert(expected == result)
    }
    "Failure decoding" in {
      val expected = Left(DecoderErr("Unable to find value at key = 3"))
      val result = Decoder.DocDecoderOps(map).decodeMap("3")
      assert(expected == result)
    }
  }

}
