package utils

import org.scalatest.FunSuite
import utils.FakeDatabase._

class FakeDatabaseTest extends FunSuite {

  test("testParticipateToSurvey") {
    val user = User(100,"testName",false,false)
    participateToSurvey(1,user)
    val condition = surveys.filter(_.id==1).map(_.participants.last).head == user
    assert(condition)
  }

  test("testRemoveUser") {
    removeUser(1)
    val condition = !users.exists(_.id == 1)
    assert(condition)
  }

  test("testNewSurvey") {
    newSurvey(100,"Q1","OPT1","OPT2",List())
    val condition = surveys.exists(_.id == 100)
    assert(condition)
  }

  test("testGetWinner") {
    removeUser(1)
    val condition = !users.exists(_.id == 1)
    assert(condition)
  }

  test("testTip") {
    tip("Jean",2)
    val condition = tips.exists(tip => tip.name == "Jean" && tip.amount == 2.0)
    assert(condition)
  }

  test("testDeleteSurvey") {
    deleteSurvey(1)
    val condition = !surveys.exists(_.id == 1)
    assert(condition)
  }

  test("testGetSubs") {
    val list = getSubs()
    val listSubs = users.count(_.subscribed)
    val condition = list.length == listSubs
    assert(condition)
  }

  test("testRemoveGA") {
    removeGA(1)
    val condition = !giveaways.exists(_.id == 1)
    assert(condition)
  }

   test("testGetTipPerUser") {
    val listTips = getTipPerUser()
    val condition = tips.filter(_.name=="Jean").map(_.amount).sum ==15
    assert(condition)
  }

  test("testNewGA") {
    newGA(100,"test",100,List())
    val condition = giveaways.exists(_.id == 100)
    assert(condition)
  }


  test("testSubToGA") {
    val user = User(100,"testName",false,false)
    subToGA(1,user)
    val condition = giveaways.filter(_.id==1).map(_.participants.last).head == user
    assert(condition)
  }

  test("testTipSum") {
    val tips = tipSum()
    val condition = tips.equals(39.0)
    assert(condition)
  }

  test("testGetSurveyResult") {
    val survey = getSurveyResult(1)
    val condition = survey.filter(_.id == 1) == survey
    assert(condition)
  }

  test("testDeleteTip") {
    deleteTip(1)
    val condition = !tips.exists(_.id == 1)
    assert(condition)
  }

  test("testGetTipByUser") {
    val tips = getTipByUser("Jean")
    val condition = tips==15.0
    assert(condition)
  }

  test("testFetchAllDonators") {
    val donnators = fetchAllDonators()
    val condition = donnators.length == 4
    assert(condition)
  }

  test("testBlockUser") {
    blockUser(1)
    val condition = users.exists(x => x.id == 1 && x.blocked)
    assert(condition)
  }

}
