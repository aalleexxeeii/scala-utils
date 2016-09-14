package com.github.aalleexxeeii.util.rule

import com.typesafe.config.Config

import scala.collection.JavaConverters._

case class Test(config: Config) {
  val map = config.entrySet().asScala.map(e ⇒ e.getKey → e.getValue).toMap
}
