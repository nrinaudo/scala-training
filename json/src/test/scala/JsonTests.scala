package com.jpmc.json

import com.jpmc.json.Json.{JNull, prettyPrintDoc, prettyPrintDocRemoveNulls}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class JsonTests extends AnyFunSuite with ScalaCheckPropertyChecks with Matchers with TestHelper {
  test("prettyPrint with nulls") {
    prettyPrintDoc(jsonData) should be(
      """{"name": "Jason","age": 13,"accounts": [122, null],"address": {"number": 29,"city": "City of JSON","country": "Serial Country","county": null,"street": "Jason St"}}"""
    )
  }

  test("prettyPrint without nulls") {
    prettyPrintDocRemoveNulls(jsonData) should be(
      """{"name": "Jason", "age": 13, "accounts": [122], "address": {"number": 29, "city": "City of JSON", "country": "Serial Country", "street": "Jason St"}}"""
    )
  }
}

trait TestHelper {
  val jsonData: Json.JDoc = Json(Map(
    "name" -> Json("Jason"),
    "age" -> Json(13),
    "accounts" -> Json(List(Json(122), JNull)),
    "address" -> Json(Map(
      "number" -> Json(29),
      "street" -> Json("Jason St"),
      "city" -> Json("City of JSON"),
      "county" -> JNull,
      "country" -> Json("Serial Country")
    ))
  ))
}