package com.jpmc.json

import org.scalatest.freespec.AnyFreeSpec
import Json._

class JsonTests extends AnyFreeSpec {
  "Pretty printing of Docs with Nulls" - {
    "Object works" in {
      val jsonObject = Object(Map(
        "name" -> String("Monica"),
        "age" -> Number(24),
        "items" -> Array(List(String("glasses"), Null, Boolean(true))),
        "addressInfo" -> Object(Map(
          "streetName" -> String("Sun Street"),
          "country" -> String("UK"),
          "county" -> Null
        ))
      ))
      val printed = prettyPrint(jsonObject)
      val expected = """{"name": "Monica", "age": 24, "items": ["glasses", null, true], "addressInfo": {"streetName": "Sun Street", "country": "UK", "county": null}}"""
      assert(printed == expected)
    }
    "Array works" in {
      val jsonArray = Array(List(Number(123), Boolean(false), String("Hello arr!"), Null))
      val printed = prettyPrint(jsonArray)
      val expected = """[123, false, "Hello arr!", null]"""
      assert(printed == expected)
    }
  }
  "Pretty printing of Docs with Nulls removed" - {
    val jsonNulls = Object(Map(
      "obj1" -> Object(Map(
        "null1" -> Null,
        "obj2" -> Object(Map(
          "null2" -> Null,
          "list" -> Array(List(Null, String("entryToList")))
        ))
      ))
    ))
    val json = Object(Map(
      "obj1" -> Object(Map(
        "obj2" -> Object(Map(
          "list" -> Array(List(Null, String("entryToList")))
        ))
      ))
    ))
    val removed = removeNullValues(jsonNulls)
    "removing nulls works" in {
      assert(removed == json)
    }
    "printing works" in {
      val printed = prettyPrint(removed)
      val expected = """{"obj1": {"obj2": {"list": [null, "entryToList"]}}}"""
      assert(printed == expected)
    }
  }
}