package com.github.aalleexxeeii.util.serialization

import play.api.libs.json._

import scala.reflect.runtime.universe._

object PlayJson {
  private val mirror = runtimeMirror(getClass.getClassLoader)

  def formatCaseObjectEnums[T: WeakTypeTag](enums: T*): Format[T] = new Format[T] {
    val tag = implicitly[WeakTypeTag[T]]
    val forward = enums.map(e ⇒
      // mirror.reflectClass(mirror.classSymbol(e.getClass)).symbol.name.decodedName.toString → e
      e.toString → e
    ).toMap
    val backward = forward.map(e ⇒ e._2 → e._1)


    override def writes(o: T): JsValue = {
      backward.get(o).map(s ⇒ JsString(s)).getOrElse(sys.error(s"$o is not enumerated for $tag"))
    }

    override def reads(json: JsValue): JsResult[T] = json match {
      case JsString(s) ⇒ forward.get(s).map(JsSuccess(_)).getOrElse(JsError(s"unknown value '$s' for enumeration $tag"))
      case wrong ⇒ JsError(s"type $tag should have string representation while it is $wrong")
    }
  }
}

object Sample {

  sealed trait Base

  case object One extends Base

  case object Two extends Base

  val Three = new Base {}
}

case object Four extends Sample.Base

object Test extends App {
  implicit val format = PlayJson.formatCaseObjectEnums[Sample.Base](Sample.One, Sample.Two, Sample.Three, Four)

  val result1 = Json.fromJson[Sample.Base](JsString("Two"))

  val result2 = Json.toJson(Four)

}
