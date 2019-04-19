import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.server.Route
import spray.json.DefaultJsonProtocol._
import utils.FakeDatabase._

import scala.io.StdIn

object WebServer {

  def main(args: Array[String]) {

    // needed to run the route
    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    // formats for unmarshalling and marshalling
    implicit val userFormat = jsonFormat4(User)
    implicit val tipFormat = jsonFormat3(Tip)
    implicit val giveAwayFormat = jsonFormat4(GiveAway)
    implicit val surveyFormat = jsonFormat7(Survey)
    implicit val listUserFormat = jsonFormat1(ListUser)
    implicit val sumTipFormat = jsonFormat1(SumTip)
    implicit val sumTipPerUserFormat = jsonFormat1(ListTips)
    implicit val UserGaFormat = jsonFormat2(UserGa)
    implicit val UserSuFormat = jsonFormat2(UserSu)


    val route: Route =
      get {
        path("donators") {
          val maybeDonator: List[String] = fetchAllDonators()
          maybeDonator match {
            case l: List[String] => {
              val lu = ListUser(l)
              complete(lu)
            }
            case _ => complete(StatusCodes.NotFound)
          }
        }
      } ~
        post {
          path("tip") {
            entity(as[Tip]) { t =>
              val tiped = tip(t.name, t.amount)
              tiped match {
                case l: Unit => complete("tip sended successfully")
                case _ => complete(StatusCodes.NotFound)
              }
            }
          }
        } ~
        get {
          path("deleteTip" / IntNumber) {
            id =>
              deleteTip(id)
              complete("Delete done")
          }
        } ~
        get {
          path("allTips") {
            val sum: Double = tipSum()
            sum match {
              case s: Double => val stip = SumTip(s)
                complete(stip)
              case _ => complete(StatusCodes.NotFound)
            }
          }
        } ~
        get {
          path("tips" / Segment) {
            name =>
              //val user = isExist(tips,id)
              val tip: SumTip = SumTip(getTipByUser(name))
              if (tip.sumTip != 0D)
                complete(tip)
              else
                complete("Tiper not found")
          }
        } ~
        get {
          path("tipsPerUser") {
            val tips: List[(String, Double)] = getTipPerUser().toList
            tips match {
              case l: List[(String, Double)] => {
                val lu = ListTips(l)
                complete(lu)
              }
              case _ => complete(StatusCodes.NotFound)
            }
            complete(tips)
          }
        }~
       get {
         path("subs") {
             val subs: List[String] = getSubs()
             subs match {
               case subss: List[String] => complete(subs)
               case _ => complete(StatusCodes.NotFound)
             }
         }
       }~
       post {
         path("newGa") {
           entity(as[GiveAway]) { item =>
             val ga:Unit  = newGA(item.id, item.event,item.amount,item.participants)
             ga match {
               case ga:Unit => complete("new giveaway created")
               case _ => complete(StatusCodes.NotFound)
             }
           }
         }
       }~
       post {
         path("subToGa") {
           entity(as[UserGa]) { item =>
             val added : Unit = subToGA(item.idGa, item.user)
             added match {
               case uga => complete("Suscribed to GA successfully")
               case _ => complete(StatusCodes.NotFound)
             }
           }
         }
       }~
       get {
         path("winner" / IntNumber) {
           id =>
             val winner: User = getWinner(id)
             winner match {
               case winnerr:User => complete(winnerr)
               case _ => complete(StatusCodes.NotFound)
             }
         }
       }~
       get {
         path("blockUser" / IntNumber ) {
            id =>
             val blocked: Unit = blockUser(id)
             complete("User blocked")
           }
       }~
       post {
         path("newSurvey") {
           entity(as[Survey]) { item =>
             val created  = newSurvey(surveys.length+1,item.question, item.option1, item.option2,List())
             complete("new survey created")
           }
         }
       }~
       post {
         path("participate") {
           entity(as[UserSu]) { item =>
              participateToSurvey(item.idSu, item.user)
              complete("Your participation was registred successfully")
           }
         }
       }~
       get {
         path("surveyResult" / IntNumber) {
           id =>
             val res: List[Survey] = getSurveyResult(id)
             res match {
               case ress => complete(ress)
               case _ => complete(StatusCodes.NotFound)
             }
           }
         }


   val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
   println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
   StdIn.readLine() // let it run until user presses return
   bindingFuture
     .flatMap(_.unbind()) // trigger unbinding from the port
     .onComplete(_ => system.terminate()) // and shutdown when done
 }
 }