package com.github.aalleexxeeii.util.akka.registry

import akka.actor.{Actor, ActorRef}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import com.github.aalleexxeeii.util.Extensions._
import com.github.aalleexxeeii.util.akka.registry.RegistryActor.Key
import com.github.aalleexxeeii.util.akka.registry.RegistryCoordinatorActor._
import com.github.aalleexxeeii.util.index.IndexedSet

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._

class RegistryCoordinatorActor extends Actor {

  import context.dispatcher

  implicit val timeout = Timeout(30.seconds)

  val registry = new Registry()
  val services = mutable.Map[String, ActorRef]() withGenerator (id ⇒ /* service proxy */ null.asInstanceOf[ActorRef])

  override def receive = {
    case Create(id, key) ⇒
      val entry = registry.byServiceAndKey((id, key)) getOrElse {
        val serviceActor = services(id)
        val originalSender = sender()
        val creationFuture: Future[ActorRef] = (serviceActor ? Create(id, key)).mapTo[ActorRef].transform({ actor ⇒
          self ! RegistryActorCreated(id, key, originalSender, actor)
          actor
        }, { ex ⇒
          self ! RegistryActorNotCreated(id, key, originalSender, ex)
          ex
        })
        val futureEntry: Entry = Entry(id, key, state = Right(creationFuture))
        registry += futureEntry
        futureEntry
      }

      (entry match {
        case Entry(_, _, Left(actor)) ⇒ Future.successful(actor)
        case Entry(_, _, Right(future)) ⇒ future
        case e ⇒ sys.error(s"Impossible state: $e")
      }) pipeTo sender()

    case RegistryActorCreated(service, key, sender, actor) ⇒
      removeEntry(service, key)
      registry += Entry(service, key, state = Left(actor))
    // sender ! actor
    case RegistryActorNotCreated(service, key, sender, ex) ⇒
      removeEntry(service, key)
    // sender ! Status.Failure(ex)
  }

  def removeEntry(service: String, key: Key): Unit =
    for (entry ← registry.byServiceAndKey((service, key))) registry -= entry
}

object RegistryCoordinatorActor {

  case class Entry(
    service: String,
    key: Key,
    state: Either[ActorRef, Future[ActorRef]]
  )

  class Registry extends IndexedSet[Entry] {
    val byService = multiple(_.service)
    val byServiceAndKey = unique(e ⇒ (e.service, e.key))
    val byActor = projection(new Projection(_.state, new UniqueLeftIndex[ActorRef]))

    /*protected class UniqueSomeIndex[K] extends UniqueIndex[Option[K]](replace = false) {
      override def filter(key: Option[K]): Boolean = key.nonEmpty
    }*/

    protected class UniqueLeftIndex[K] extends UniqueIndex[Either[K, _]](replace = false) {
      override def filter(key: Either[K, _]): Boolean = key.isLeft
    }

  }

  case class Create(id: String, key: Key)

  protected case class RegistryActorCreated(service: String, key: Key, sender: ActorRef, actor: ActorRef)

  protected case class RegistryActorNotCreated(service: String, key: Key, sender: ActorRef, ex: Throwable)

}

object RegistryActor {
  type Key = Serializable
}

object RegistryActorFactory {

}
