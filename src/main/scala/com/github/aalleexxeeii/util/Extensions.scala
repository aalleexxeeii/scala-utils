package com.github.aalleexxeeii.util

import scala.collection.mutable

object Extensions {
  implicit class MutableMapExtensions[K, V](val map: mutable.Map[K, V]) extends AnyVal {
      def withGenerator(f: K ⇒ V): mutable.Map[K, V] = new MutableMapWithGenerator(map, f)
    }

    private class MutableMapWithGenerator[K, V](val map: mutable.Map[K, V], val f: K ⇒ V) extends mutable.Map.WithDefault(map, f) {
      override final def default(key: K): V = {
        val d = super.default(key)
        update(key, d)
        d
      }
    }
}
