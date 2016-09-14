package com.github.aalleexxeeii.util.rule

import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.JavaConverters._

class RuleSpec extends FlatSpec with Matchers {
  val config = ConfigFactory.load("rules")
  val ruleConfig = config.getConfigList("rules")
  val configConfig = config.getConfig("cfg")
  val rules = Engine(ruleConfig, configConfig)

  val test1 = parse(
    """
      | game = chess
      | tenant = A
      | client = mobile
      | currency = JPY
    """.stripMargin)

  val result1 = rules.resolveTraceable(test1)

  println(dump(result1))

  def parse(s: String) = Test(ConfigFactory.parseString(s))

  def dump(c: Config) =
    c.origin().comments().asScala.map(s ⇒ s"# $s").mkString("\n") +
    c.root().render(ConfigRenderOptions.defaults.setOriginComments(true).setJson(false).setFormatted(true))
}
