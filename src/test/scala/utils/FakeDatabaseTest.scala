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

  test("testTips") {
    tip("Jean",2)
    val condition = tips.filter(tip => tip.name =="Jean" && tip.amount == 2)
    condition
     // ==1).map(_.participants.last).head == user
   // assert(condition)
  }

  test("testTips_$eq") {

  }

  test("testRemoveUser") {

  }

  test("testNewSurvey") {

  }

  test("testGetWinner") {

  }

  test("testTip") {

  }

  test("testDeleteSurvey") {

  }

  test("testGetSubs") {

  }

  test("testUsers") {

  }

  test("testUsers_$eq") {

  }

  test("testRemoveGA") {

  }

  test("testGetTipPerUser") {

  }

  test("testGiveaways") {

  }

  test("testGiveaways_$eq") {

  }

  test("testNewGA") {

  }

  test("testSurveys") {

  }

  test("testSurveys_$eq") {

  }

  test("testSubToGA") {

  }

  test("testTipSum") {

  }

  test("testGetSurveyResult") {

  }

  test("testDeleteTip") {

  }

  test("testGetTipByUser") {

  }

  test("testFetchAllDonators") {

  }

  test("testBlockUser") {

  }

}
