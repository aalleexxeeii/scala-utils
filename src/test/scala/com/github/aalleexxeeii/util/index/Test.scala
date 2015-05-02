package com.github.aalleexxeeii.util.index

import org.scalatest.{FlatSpec, Matchers}

class ItemIndex extends IndexedSet[Item]() {
  val byId = unique(_.id)
  val byValue = multiple(_.value)
}

class IndexedTestSpec extends FlatSpec with Matchers {
  // TODO: make specifications of it
  val indexed = new ItemIndex

  indexed += Item(1, "one", 1)
  indexed ++= Item(2, "two", 4) :: Item(3, "three", 9) :: Item(4, "four", 9) :: Nil

  indexed -= Item(2, "two", 4)

  println(indexed.keys(indexed.byId))

  println(indexed.byId(3))
  println(indexed.byValue(9))
  println(indexed.byId(2))

  indexed -= Item(3, "three", 9)

  println(indexed.byValue(9))
  println(indexed.byId(3))

}

case class Item(id: Int, name: String, value: Any)
