package com.github.aalleexxeeii.util.rule

import com.typesafe.config.{Config, ConfigFactory}

import scala.collection.JavaConverters._

case class Rules(rules: List[Rule]) {
  def matching(x: Test): List[Rule] = rules filter (_.condition matches x)

  def resolve(x: Test): Config = matching(x).foldLeft(ConfigFactory.empty())((c, r) ⇒ r.result withFallback c)

  def resolveTraceable(x: Test): Config = matching(x).foldLeft(ConfigFactory.empty()) { (config, rule) ⇒
    val merged = rule.result.withFallback(config)
    val resultComments = rule.result.origin().comments().asScala.toList
    val conditionComments = rule.condition.config.origin().comments().asScala.toList map (x ⇒ s"condition: $x")
    val originalComments = merged.origin().comments.asScala.toList
    merged.root().withOrigin(rule.result.origin().withComments(
      (conditionComments ::: resultComments ::: originalComments).asJava)
    ).toConfig
  }
}
