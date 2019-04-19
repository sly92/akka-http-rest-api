import scala.collection.immutable

final case class User(id: Int, name: String, subscribed: Boolean, blocked: Boolean)

final case class Tip(id: Int, name: String, amount: Double)

final case class GiveAway(id: Int, event: String, amount: Double, participants: List[User])

final case class Survey(id: Int, question: String, option1: String, option2: String, vote1: Int, vote2: Int, participants: List[User])

var users: List[User] = List(
  User(1, "Jean", true, true),
  User(2, "Pierre", false, false),
  User(3, "Benoit", false, false),
  User(4, "Magalie", false, false),
  User(5, "Sebastien", false, false),
  User(6, "Nicolas", false, false),
  User(7, "Coralie", false, false),
  User(8, "Adam", false, false),
  User(9, "Ahmed", false, false),
  User(10, "Beatrice", false, false),
  User(11, "Sarah", false, false)
)

var tips: List[Tip] = List(
  Tip(1, "Jean", 10),
  Tip(2, "Sebastien", 1),
  Tip(3, "Sarah", 3),
  Tip(4, "Jean", 5),
  Tip(5, "Coralie", 20)
)
var giveaways: List[GiveAway] = List(
  GiveAway(1, "Event1", 1000, List(
    User(2, "Pierre", false, false),
    User(3, "Benoit", false, false),
    User(4, "Magalie", false, false),
    User(5, "Sebastien", false, false),
    User(6, "Nicolas", false, false),
    User(7, "Coralie", false, false)
  )),
  GiveAway(2, "Event2", 500, List(
    User(2, "Pierre", false, false),
    User(3, "Benoit", false, false),
    User(4, "Magalie", false, false),
    User(5, "Sebastien", false, false)
  )),
  GiveAway(3, "Event3", 20000, List(
    User(2, "Pierre", false, false),
    User(3, "Benoit", false, false),
    User(4, "Magalie", false, false),
    User(5, "Sebastien", false, false),
    User(6, "Adam", false, false),
    User(7, "Ahmed", false, false),
    User(8, "Beatrice", false, false),
    User(9, "Sarah", false, false)
  )),
  GiveAway(4, "Event4", 100000, List[User]())
)
var surveys: List[Survey] = List(
  Survey(
    1,
    "Q1",
    "OPT1",
    "OPT2",
    10,
    20,
    List(
      User(7, "Ahmed", false, false),
      User(8, "Beatrice", false, false),
      User(9, "Sarah", false, false)
    )),
  Survey(
    2,
    "Q2",
    "OPT1",
    "OPT2",
    10,
    20,
    List(
      User(3, "Benoit", false, false),
      User(8, "Beatrice", false, false),
      User(9, "Sarah", false, false)
    )),
  Survey(
    3,
    "Q3",
    "OPT1",
    "OPT2",
    10,
    20,
    List(
      User(1, "Jean", false, false),
      User(2, "Sebastien", false, false),
      User(9, "Sarah", false, false)
    ))
)
def fetchAllDonators(): List[String] = {
  tips.map(_.name).distinct
}

/* TIPS */
def tip(name: String, amount: Double): List[Tip] = {
  if (users.exists(_.name == name))
    tips :+ Tip(tips.length + 1, name, amount)
  else
    tips
}

def deleteTip(id: Int): Unit = {
  tips= tips.filter(_.id!=id)
}

def tipSum(): Option[Double] = {
  Some(tips.map(_.amount).sum)
}

def getTipByUser(name: String):Double = {
  tips.filter(_.name==name).map(_.amount).sum
}

def getTipPerUser(): Map[String, Double] = {
  tips.groupBy(_.name).mapValues(_.map(_.amount).sum)
}

/* SUBS */
def getSubs() = {
  users.filter(_.subscribed).map(_.name)
}

/* GAs */
def newGA(id:Int,event: String, cashPrize:Double, participants:List[User]): List[GiveAway] = {
  giveaways :+ GiveAway(id, event, cashPrize, participants)
}

def removeGA(id: Int): Unit = {
  giveaways = giveaways.filter(_.id!=id)
}

def subToGA(id:Int, user: User) = {
  giveaways.find(_.id == id) match {
    case Some(g) =>
      val ga = g
      val l = g.participants ++ List(user)
      removeGA(g.id)
      giveaways = newGA(ga.id, ga.event,ga.amount,l)
    case _ => None
  }
}

def getWinner(id: Int): User = {
  val r = scala.util.Random
  val list = giveaways.filter(_.id == id)(0)
  list.participants(r.nextInt(list.participants.length))
  //list().participants
  /*  if (!list(r.nextInt(list.length)).participants.asInstanceOf[User].blocked)
      list(r.nextInt(list.length)).participants*/
}

def removeUser(id:Int): Unit = {
  users = users.filter(_.id!=id)
}
/* USERS */
def blockUser(id: Int): Unit = {
  users.find(_.id == id) match {
    case Some(u) =>
      val newU = u
      removeUser(u.id)
      users :+ (newU.id, newU.name,newU.subscribed,newU.blocked)
    case _ => None
  }
}

/* SURVEYS */
def newSurvey(id:Int,question:String,option1: String, option2: String, listParticipants:List[User]): Unit = {
  surveys = surveys :+ Survey(id,question, option1, option2, 0, 0, listParticipants)
}

def deleteSurvey(id:Int): Unit = {
  surveys = surveys.filter(_.id!=id)
}

def participateToSurvey(user: User, id: Int): Unit = {
  surveys.find(_.id == id) match {
    case Some(u) =>
      val newP = u
      val l = u.participants ++ List(user)
      deleteSurvey(u.id)
      newSurvey(newP.id,newP.question,newP.option1,newP.option2,l)
    case _ => None
  }
}

def getSurveyResult(id: Int): Any = {
  surveys.filter(_.id == id)
}

def isExist[A](as: List [ A ], item:A): Boolean ={
  as.contains(item)
}

isExist(users,1)

