package com.github.aalleexxeeii.util.index

import scala.collection.generic.{GenericSetTemplate, MutableSetFactory}
import scala.collection.mutable
import scala.language.higherKinds


class IndexedSet[E](val u: mutable.Set[E] = mutable.Set[E]())
  extends mutable.AbstractSet[E]
  with mutable.SetLike[E, IndexedSet[E]]
  with mutable.Set[E]
  with GenericSetTemplate[E, IndexedSet] {
  override def empty: IndexedSet[E] = new IndexedSet(u.empty)

  override def +=(elem: E) = {
    u += elem
    for (p ← _projections) {
      val key = p.projection(elem)
      p.index.put(key, elem)
    }
    this
  }

  override def -=(elem: E) = {
    u -= elem
    for (p ← _projections) {
      val key = p.projection(elem)
      p.index.remove(key, elem)
    }
    this
  }

  override def contains(elem: E) = u.contains(elem)

  override def seq = u.seq

  override def iterator = u.iterator

  override def companion = IndexedSet

  protected val _projections = mutable.Buffer[Projection[Any, Any]]()

  protected def projection[K, C[_]](p: Projection[K, C]): K ⇒ C[E] = {
    _projections += p.asInstanceOf[Projection[Any, Any]]
    // key ⇒ p.index.get(key)
    new IndexFunction(p)
  }

  protected def single[K](f: E ⇒ K): K ⇒ Option[E] = {
    projection(new Projection(f, new UniqueIndex[K]()))
  }

  protected def unique[K](f: E ⇒ K): K ⇒ Option[E] = {
    projection(new Projection(f, new UniqueIndex[K](replace = false)))
  }

  protected def multiple[K](f: E ⇒ K): K ⇒ collection.Set[E] = {
    projection(new Projection(f, new MultiIndex[K]()))
  }

  protected class Projection[K, C[_]](val projection: E ⇒ K, val index: Index[K, C])

  protected trait Index[K, C[_]] {
    def get(key: K): C[E]

    def put(key: K, value: E): Unit

    def remove(key: K, value: E): Unit

    def keys: collection.Set[K]
  }

  protected class UniqueIndex[K](val replace: Boolean = true) extends Index[K, Option] {
    protected val index = mutable.Map[K, E]()

    override def get(key: K): Option[E] = {
      index.get(key)
    }

    override def put(key: K, value: E): Unit =
      if (index.contains(key) && !replace) sys.error(s"Duplicate index $key")
      else index(key) = value

    override def remove(key: K, value: E): Unit = index.remove(key)

    override def keys = index.keySet
  }

  protected class MultiIndex[K]() extends Index[K, mutable.Set] {
    protected val index = new mutable.HashMap[K, mutable.Set[E]] with mutable.MultiMap[K, E]

    override def get(key: K): mutable.Set[E] = index.getOrElse(key, mutable.Set.empty)

    override def put(key: K, value: E): Unit = index.addBinding(key, value)

    override def remove(key: K, value: E): Unit = index.removeBinding(key, value)

    override def keys: collection.Set[K] = index.keySet
  }

  class IndexFunction[K, C[_]](val p: Projection[K, C]) extends ((K) ⇒ C[E]) {
    override def apply(key: K): C[E] = p.index.get(key)
  }

  def keys[K, C[_]](projection: (K) ⇒ C[E]): collection.Set[K] = projection match {
    case f: IndexFunction[_, _] ⇒ f.asInstanceOf[IndexFunction[K, Any]].p.index.keys
    case wrong ⇒ sys.error(s"Incorrect index function ${wrong.getClass} passed")
  }
}

object IndexedSet extends MutableSetFactory[IndexedSet] {
  def apply[E](underlying: mutable.Set[E] = mutable.Set[E]()) = new IndexedSet(underlying)
}


class ItemIndex extends IndexedSet[Item]() {
  val byId = unique(_.id)
  val byValue = multiple(_.value)
}

object Test extends App {
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