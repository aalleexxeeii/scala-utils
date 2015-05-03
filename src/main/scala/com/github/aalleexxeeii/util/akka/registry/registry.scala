package com.github.aalleexxeeii.util.akka.registry

import akka.actor.{Actor, ActorLogging, ActorRef, Terminated}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import com.github.aalleexxeeii.util.Extensions._
import com.github.aalleexxeeii.util.akka.registry.RegistryActor.Key
import com.github.aalleexxeeii.util.akka.registry.RegistryCoordinatorActor._
import com.github.aalleexxeeii.util.index.IndexedSet

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class RegistryCoordinatorActor extends Actor with ActorLogging {

  import context.dispatcher

  implicit val timeout = Timeout(30.seconds)

  val registry = new Registry()
  val services = mutable.Map[String, ActorRef]() withGenerator (id ⇒ /* service proxy */ null.asInstanceOf[ActorRef])

  override def receive = {
    case Create(id, key) ⇒
      val entry = registry.byServiceAndKey((id, key)) getOrElse {
        val serviceActor = services(id)
        val creationFuture = (serviceActor ? Create(id, key)).mapTo[ActorRef]
        creationFuture onComplete {
          case Success(actor) ⇒
            self ! RegistryActorCreated(id, key, actor)
            context watch actor
          case Failure(ex) ⇒
            self ! RegistryActorNotCreated(id, key, ex)
        }
        val futureEntry = Entry(id, key, state = Right(creationFuture))
        registry += futureEntry
        futureEntry
      }

      (entry.state match {
        case Left(actor) ⇒ Future.successful(actor)
        case Right(future) ⇒ future
      }) pipeTo sender()

    case RegistryActorCreated(service, key, actor) ⇒
      removeEntry(service, key)
      registry += Entry(service, key, state = Left(actor))

    case RegistryActorNotCreated(service, key, ex) ⇒
      removeEntry(service, key)

    case Terminated(actor) ⇒
      registry.byActor(actor) match {
        case Some(entry) ⇒
          registry -= entry
          log.debug(s"Registry actor for $entry terminated")
        case None ⇒
          log.warning(s"Unknown actor terminated: $actor")
      }
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
    val byActorOption = projection(new Projection(_.state.left.toOption, new UniqueSomeIndex[ActorRef]))

    def byActor(actor: ActorRef): Option[Entry] = byActorOption(Some(actor))

    protected class UniqueSomeIndex[K] extends UniqueIndex[Option[K]](replace = false) {
      override def filter(key: Option[K]): Boolean = key.nonEmpty
    }

    protected class UniqueLeftIndex[K] extends UniqueIndex[Either[K, _]](replace = false) {
      override def filter(key: Either[K, _]): Boolean = key.isLeft
    }

  }

  case class Create(id: String, key: Key)

  protected case class RegistryActorCreated(service: String, key: Key, actor: ActorRef)

  protected case class RegistryActorNotCreated(service: String, key: Key, ex: Throwable)

}

object RegistryActor {
  type Key = Serializable
}

object RegistryActorFactory {

}
