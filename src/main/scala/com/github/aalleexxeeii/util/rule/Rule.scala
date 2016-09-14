package com.github.aalleexxeeii.util.rule

import com.typesafe.config.{Config, ConfigValueType}

import scala.collection.JavaConverters._

case class Rule(condition: Condition, result: Config) {

}

object Rule {
  def apply(config: Config, configs: Config): Rule = Rule(
    condition = {
      val c = config.getConfig(IF)

      Condition(
        c.root().withOrigin(
          c.origin().withComments(
            (config.root().origin().comments().asScala ++ c.origin().comments().asScala).asJava
          )
        ).toConfig
      )
    },
    result = config.getValue(THEN).valueType match {
      case ConfigValueType.STRING ⇒ configs.getConfig(config.getString(THEN))
      case ConfigValueType.OBJECT ⇒ config.getConfig(THEN)
      case x ⇒ sys.error(s"'then' value should be either a string or an object while it is $x")
    }
  )

  final val IF = "if"
  final val THEN = "then"
}
