package com.github.aalleexxeeii.util.rule

import java.{util ⇒ ju}

import com.typesafe.config.Config

import scala.collection.JavaConverters._

object Engine {
  def apply(conditions: ju.List[_ <: Config], configs: Config): Rules = Rules(
    conditions.asScala.toList map (Rule(_, configs))
  )

  /*{
  case co: ConfigObject ⇒ Rule(co.toConfig, configs)
  case x ⇒ sys.error(s"$x is not an object")
  }*/

}
