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
        val futureEntry: Entry = Entry(id, key, actor = None, Some(creationFuture))
        registry += futureEntry
        futureEntry
      }

      (entry match {
        case Entry(_, _, Some(actor), _) ⇒ Future.successful(actor)
        case Entry(_, _, None, Some(future)) ⇒ future
        case e ⇒ sys.error(s"Impossible state: $e")
      }) pipeTo sender()

    case RegistryActorCreated(service, key, sender, actor) ⇒
      for (entry ← registry.byServiceAndKey((service, key))) registry -= entry
      registry += Entry(service, key, actor = Some(actor), None)
    // sender ! actor
    case RegistryActorNotCreated(service, key, sender, ex) ⇒
      for (entry ← registry.byServiceAndKey((service, key))) registry -= entry
    // sender ! Status.Failure(ex)
  }
}

object RegistryCoordinatorActor {

  case class Entry(
    service: String,
    key: Key,
    actor: Option[ActorRef] = None,
    actorFuture: Option[Future[ActorRef]] = None
  )


  class Registry extends IndexedSet[Entry] {
    val byService = multiple(_.service)
    val byServiceAndKey = unique(e ⇒ (e.service, e.key))
    val byActor = projection(new Projection(_.actor, new UniqueSomeIndex[ActorRef]))

    protected class UniqueSomeIndex[K] extends UniqueIndex[Option[K]](replace = false) {
      override def filter(key: Option[K]): Boolean = key.nonEmpty
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
