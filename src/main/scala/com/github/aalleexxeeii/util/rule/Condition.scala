package com.github.aalleexxeeii.util.rule

import com.typesafe.config.Config

import scala.collection.JavaConverters._

case class Condition(config: Config) {
  val map = config.entrySet().asScala.map(e ⇒ e.getKey → e.getValue).toMap

  def matches(x: Test) = map forall { case (k, v) ⇒ x.map get k contains v }
}
