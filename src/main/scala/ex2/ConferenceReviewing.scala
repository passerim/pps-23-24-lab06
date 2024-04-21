package ex2

import ex2.ConferenceReviewing.Question

trait ConferenceReviewing:
  /** @param article
    * @param scores
    *   loads a review for the specified article, with complete scores as a map
    */
  def loadReview(article: Int, scores: Map[Question, Int]): Unit

  /** @param article
    * @param relevance
    * @param significance
    * @param confidence
    * @param fin
    *   loads a review for the specified article, with the 4 explicit scores
    */
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

  /** @param article
    * @param question
    * @return
    *   the scores given to the specified article and specified question, as an (ascending-ordered)
    *   list
    */
  def orderedScores(article: Int, question: Question): List[Int]

  /** @param article
    * @return
    *   the average score to question FINAL taken by the specified article
    */
  def averageFinalScore(article: Int): Double

  /** An article is considered accepted if its averageFinalScore (not weighted) is > 5, and at least
    * one RELEVANCE score is >= 8.
    *
    * @return
    *   the set of accepted articles
    */
  def acceptedArticles: Set[Int]

  /** @return
    *   accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best
    *   based on averageFinalScore
    */
  def sortedAcceptedArticles: List[(Int, Double)]

  /** @return
    *   a map from articles to their average "weighted final score", namely, the average value of
    *   CONFIDENCE*FINAL/10
    */
  def averageWeightedFinalScoreMap: Map[Int, Double]

object ConferenceReviewing:
  /** For each article, the reviewer has to reply to all the following questions
    */
  enum Question:
    case Relevance    // Is it important for this conference?
    case Significance // Does it produce a scientific contribution?
    case Confidence   // Do you feel competent to comment on it?
    case Final        // Is this an item worth accepting?

  def apply(): ConferenceReviewing = ConferenceReviewingImpl()

  private class ConferenceReviewingImpl extends ConferenceReviewing:
    private var reviews: List[(Int, Map[Question, Int])] = List.empty

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      if (scores.size < Question.values.length) throw new IllegalArgumentException
      else reviews = (article, scores) :: reviews

    override def loadReview(
        article: Int,
        relevance: Int,
        significance: Int,
        confidence: Int,
        fin: Int
    ): Unit =
      loadReview(
        article,
        Map(
          Question.Relevance    -> relevance,
          Question.Significance -> significance,
          Question.Confidence   -> confidence,
          Question.Final        -> fin
        )
      )

    override def orderedScores(article: Int, question: Question): List[Int] =
      reviews
        .collect({ case (a, m) if a == article && m.contains(question) => m(question) })
        .sortWith(_ < _)

    override def averageFinalScore(article: Int): Double =
      val scores = reviews
        .collect({
          case (a, m) if a == article && m.contains(Question.Final) => m(Question.Final).toDouble
        })
      scores.sum / scores.length

    private def accepted(article: Int): Boolean =
      averageFinalScore(article) > 5.0 && reviews
        .collect({
          case (a, m) if a == article && m.contains(Question.Relevance) => m(Question.Relevance)
        })
        .exists(_ >= 8)

    override def acceptedArticles: Set[Int] =
      reviews.map(_._1).distinct.filter(accepted).toSet

    override def sortedAcceptedArticles: List[(Int, Double)] =
      acceptedArticles.toList.map(id => (id, averageFinalScore(id))).sortWith(_._2 < _._2)

    private def averageWeightedFinalScore(article: Int): Double =
      val scores = reviews
        .collect({
          case (a, m)
              if a == article && m.contains(Question.Final) && m.contains(Question.Confidence) =>
            (m(Question.Final) * m(Question.Confidence)).toDouble / 10.0
        })
      scores.sum / scores.length

    override def averageWeightedFinalScoreMap: Map[Int, Double] =
      reviews.map(_._1).distinct.map(id => (id, averageWeightedFinalScore(id))).toMap
