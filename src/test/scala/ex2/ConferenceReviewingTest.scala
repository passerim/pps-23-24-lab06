package ex2

import ex2.ConferenceReviewing.Question
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.{BeforeEach, Test}

import scala.collection.mutable

/** See the documentation for the ConferenceReviewing interface, which models the results of the
  * process of reviewing conference papers. Each article is reviewed by one or more anonymous
  * reviewers, each of whom provides an evaluation (score) from 0 to 10 out of 4 several
  * "questions", modeled by ConferenceReviewing.Question. An article is accepted if the average
  * rating value for the "FINAL" question is >5 and if it has at least one rating "RELEVANCE" >= 8.
  *
  * Implement ConferenceReviewing through a ConferenceReviewingImpl class with constructor without
  * arguments, so that it passes all tests below, made to be self-explanatory.
  */

class ConferenceReviewingTest:
  var cr: ConferenceReviewing = ConferenceReviewing()

  @BeforeEach def init(): Unit =
    cr = ConferenceReviewing()
    cr.loadReview(1, 8, 8, 6, 8)
    cr.loadReview(1, 9, 9, 6, 9)
    cr.loadReview(2, 9, 9, 10, 9)
    cr.loadReview(2, 4, 6, 10, 6)
    cr.loadReview(3, 3, 3, 3, 3)
    cr.loadReview(3, 4, 4, 4, 4)
    cr.loadReview(4, 6, 6, 6, 6)
    cr.loadReview(4, 7, 7, 8, 7)
    val map = mutable.Map[Question, Int]()
    map += (Question.Relevance    -> 8)
    map += (Question.Significance -> 8)
    map += (Question.Confidence   -> 7)
    map += (Question.Final        -> 8)
    cr.loadReview(4, map.toMap)
    cr.loadReview(5, 6, 6, 6, 10)
    cr.loadReview(5, 7, 7, 7, 10)

  @Test def testOrderedScores(): Unit =
    assertEquals(cr.orderedScores(2, Question.Relevance), List(4, 9))
    assertEquals(cr.orderedScores(4, Question.Confidence), List(6, 7, 8))
    assertEquals(cr.orderedScores(5, Question.Final), List(10, 10))

  @Test def testAverageFinalScore(): Unit =
    assertEquals(cr.averageFinalScore(1), 8.5, 0.01)
    assertEquals(cr.averageFinalScore(2), 7.5, 0.01)
    assertEquals(cr.averageFinalScore(3), 3.5, 0.01)
    assertEquals(cr.averageFinalScore(4), 7.0, 0.01)
    assertEquals(cr.averageFinalScore(5), 10.0, 0.01)

  @Test def testAcceptedArticles(): Unit =
    assertEquals(cr.acceptedArticles, Set(1, 2, 4))

  @Test def testSortedAcceptedArticles(): Unit =
    assertEquals(cr.sortedAcceptedArticles, List((4, 7.0), (2, 7.5), (1, 8.5)))

  @Test def optionalTestAverageWeightedFinalScore(): Unit =
    assertEquals(cr.averageWeightedFinalScoreMap.getOrElse(1, -1.0), (4.8 + 5.4) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap.getOrElse(2, -1.0), (9.0 + 6.0) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap.getOrElse(3, -1.0), (0.9 + 1.6) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap.getOrElse(4, -1.0), (3.6 + 5.6 + 5.6) / 3, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap.getOrElse(5, -1.0), (6.0 + 7.0) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap.size, 5)
