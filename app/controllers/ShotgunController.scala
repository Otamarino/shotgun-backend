package controllers

import java.net.URL
import javax.inject._

import play.api._
import play.api.mvc._
import play.api.libs.json._

import scala.concurrent.duration.FiniteDuration

@Singleton
class ShotgunController @Inject() extends Controller {
  implicit val carFormat = Json.format[Car]
  implicit val userFormat = Json.format[User]
  implicit val shotgunFormat = Json.format[Shotgun]
  implicit val disputeFormat = Json.format[Dispute]

  case class Car(id: Long, owner: User, plate: String, model: String, free: Boolean, disputed: Boolean, photo: URL)
  case class User(id: Long, name: String, score: Int, photo: URL)
  case class Shotgun(id: Long, user: User, time: FiniteDuration, car: Car)
  case class Dispute(id: Long, reason: String, user: User, choice: DisputeChoice, shotgun: Shotgun)

  trait DisputeChoice {
    //(winner, why)
    def battle(against: DisputeChoice): (DisputeChoice, String)
  }
  case object Rock extends DisputeChoice {
    override def battle(against: DisputeChoice): (Option[DisputeChoice], String) = against match {
      case Rock => (None, "Tie")
      case Paper => (Some(Paper), "covers")
      case Scissors => (Some(Rock), "crushes")
      case Lizard => (Some(Rock), "crushes")
      case Spock => (Some(Spock), "vaporizes")
    }
  }
  case object Paper extends DisputeChoice {
    override def battle(against: DisputeChoice): (Option[DisputeChoice], String) = against match {
      case Rock => (Some(Paper), "covers")
      case Paper => (None, "Tie")
      case Scissors => (Some(Scissors), "cuts")
      case Lizard => (Some(Lizard), "eats")
      case Spock => (Some(Paper), "disproves")
    }
  }
  case object Scissors extends DisputeChoice {
    override def battle(against: DisputeChoice): (Option[DisputeChoice], String) = against match {
      case Rock => (Some(Rock), "crushes")
      case Paper => (Some(Scissors), "cuts")
      case Scissors => (None, "Tie")
      case Lizard => (Some(Scissors), "decapitates")
      case Spock => (Some(Spock), "smashes")
    }
  }
  case object Lizard extends DisputeChoice {
    override def battle(against: DisputeChoice): (Option[DisputeChoice], String) = against match {
      case Rock => (Some(Rock), "crushes")
      case Paper => (Some(Lizard), "eats")
      case Scissors => (Some(Scissors), "decapitates")
      case Lizard => (None, "Tie")
      case Spock => (Some(Spock), "poisons")
    }
  }
  case object Spock extends DisputeChoice {
    override def battle(against: DisputeChoice): (Option[DisputeChoice], String) = against match {
      case Rock => (Some(Spock), "vaporizes")
      case Paper => (Some(Paper), "disproves")
      case Scissors => (Some(Spock), "smashes")
      case Lizard => (Some(Spock), "poisons")
      case Spock => (None, "Tie")
    }
  }

  var cars = Map.empty[Long, Car]
  var users = Map.empty[Long, User]
  var shotguns = Map.empty[Long, Shotgun]
  var disputes = Map.empty[Long, Dispute]

  def leaderboard() = Ok(Json.toJson(users.values.toArray))
  def getUser(userId: Long) = users.get(userId) match {
    case Some(u) => Ok(Json.toJson(u))
    case None => NotFound(Json.obj("message" -> s"User with Id = $userId not found."))
  }

  def getCars = Ok(Json.toJson(cars.values.toArray))
  def getCar(carId: Long) = cars.get(carId) match {
    case Some(c) => Ok(Json.toJson(c))
    case None => NotFound(Json.obj("message" -> s"Car with Id = $carId not found."))
  }
  def addCar() = play.mvc.Results.TODO

  def shotgun() = Action(parse.json) { request =>
    val userId = (request.body \ "userId").as[Long]
    val carId = (request.body \ "carId").as[Car]


  }

  def getShotgun(shotgunId: Long) = shotguns.get(shotgunId) match {
    case Some(s) => Ok(Json.toJson(s))
    case None => NotFound(Json.obj("message" -> s"Shotgun with Id = $shotgunId not found."))
  }

  def createReason(id: Long, message: String) = Json.obj(
    "id" -> id,
    "message" -> message
  )
  def reasons = Ok(Json.arr(
    createReason(0, "Had no footwear."),
    createReason(1, "Went back inside."),
    createReason(2, "Not in line of sight."),
    createReason(3, "No reload."),
    createReason(4, "I WANT shotgun.")
  ))

  def dispute() = play.mvc.Results.TODO

  def getDispute(disputeId: Long) = disputes.get(disputeId) match {
    case Some(d) => Ok(Json.toJson(d))
    case None => NotFound(Json.obj("message" -> s"Dispute with Id = $disputeId not found."))
  }

  def gameDispute(disputeId: Long) = play.mvc.Results.TODO
}
