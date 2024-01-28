package com.jpmc.json

import com.jpmc.json.Json._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class JsonTests extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers with TestHelper {
  "calling removeNull on JSON data" should {
    "remove JSON key-values whose values are JSON Null" in {
      removeNull(jsonData) should be(jsonDataWithoutNulls)
    }
  }

  "calling prettyPrint on JSON data" should {
    "create string representation of JSON data" in {
      prettyPrint(jsonData) should be(jsonInString)
    }
  }

  "calling skipNullPrettyPrint on JSON data" should {
    "remove JSON key-values whose values are JSON Null and create string representation of JSON data" in {
      skipNullPrettyPrint(jsonData) should be(jsonInStringWithNulls)
    }
  }
}

trait TestHelper {
  val jsonData: Doc = Json(Map(
    "name" -> Json("Jason"),
    "age" -> Json(13),
    "gender" -> Null,
    "accounts" -> Json(List(Json(122), Null)),
    "address" -> Json(Map(
      "number" -> Json(29),
      "street" -> Json("Jason St"),
      "city" -> Json("City of JSON"),
      "county" -> Null,
      "country" -> Json("Serial Country")
    ))
  ))

  val jsonDataWithoutNulls: Doc = Json(Map(
    "name" -> Json("Jason"),
    "age" -> Json(13),
    "accounts" -> Json(List(Json(122), Null)),
    "address" -> Json(Map(
      "number" -> Json(29),
      "street" -> Json("Jason St"),
      "city" -> Json("City of JSON"),
      "country" -> Json("Serial Country")
    ))
  ))

  val jsonInString = """{"name": "Jason","age": 13,"address": {"number": 29,"city": "City of JSON","country": "Serial Country","county": null,"street": "Jason St"},"accounts": [122, null],"gender": null}"""
  val jsonInStringWithNulls = """{"name": "Jason","age": 13,"address": {"number": 29,"city": "City of JSON","country": "Serial Country","street": "Jason St"},"accounts": [122, null]}"""
}
