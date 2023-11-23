package day02

package day02

import scala.io.Source

object Main {

  def main(args: Array[String]) = {
    val guide = Source
      .fromResource("days01_10/day02_input.txt")
      .getLines()
      .map(line => line.split(" "))
      .map(entry => (opponentMove(entry(0)), playerMove(entry(1))))
      .toList

    val totalScore = guide
      .map(x => moveMoveScenarios(x._1, x._2))
      .map(_.sum)
      .sum

    println(s"Part 1: Guide total is: ${totalScore} points")

    val updatedScore = guide
      .map(x => (x._1, playerMoveToOutcome(x._2)))
      .map(x => moveOutcomeScenarios(x._1, x._2))
      .map(_.sum)
      .sum

    println(s"Part 2: Fixed total is: ${updatedScore} points")
  }

  // Moves
  val ROCK     = "Rock"
  val PAPER    = "Paper"
  val SCISSORS = "Scissors"
  val WIN      = "Win"
  val DRAW     = "Draw"
  val LOSE     = "Lose"
  // alias for easier reading
  val PLAYER_WANTS_TO_WIN_AGAINST  = WIN
  val PLAYER_WANTS_TO_LOSE_AGAINST = LOSE
  val PLAYER_WANTS_TO_DRAW_AGAINST = DRAW

  def opponentMove = Map(
    "A" -> ROCK,
    "B" -> PAPER,
    "C" -> SCISSORS
  )

  def playerMove = Map(
    "X" -> ROCK,
    "Y" -> PAPER,
    "Z" -> SCISSORS
  )

  def playerMoveToOutcome = Map(
    ROCK     -> LOSE,
    PAPER    -> DRAW,
    SCISSORS -> WIN
  )

  def moveMoveScenarios(oppMove: String, playerMove: String): List[Int] = {
    val gameScore = (playerMove, oppMove) match {
      case (ROCK, PAPER) | (PAPER, SCISSORS) | (SCISSORS, ROCK) => LOSE
      case (ROCK, SCISSORS) | (PAPER, ROCK) | (SCISSORS, PAPER) => WIN
      case (_, _)                                               => DRAW
    }

    val playScore = playerMove
    List(gameScore, playScore).map(score)
  }

  def moveOutcomeScenarios(oppMove: String, desiredOutcome: String): List[Int] = {
    val gameScore = desiredOutcome
    val playScore = (desiredOutcome, oppMove) match {
      case (PLAYER_WANTS_TO_WIN_AGAINST, ROCK) | (PLAYER_WANTS_TO_LOSE_AGAINST, SCISSORS) => PAPER
      case (PLAYER_WANTS_TO_LOSE_AGAINST, ROCK) | (PLAYER_WANTS_TO_WIN_AGAINST, PAPER) => SCISSORS
      case (PLAYER_WANTS_TO_LOSE_AGAINST, PAPER) | (PLAYER_WANTS_TO_WIN_AGAINST, SCISSORS) => ROCK
      case (PLAYER_WANTS_TO_DRAW_AGAINST, anything) => anything
    }
    List(gameScore, playScore).map(score)
  }

  def score(phrase: String): Int = phrase match {
    case ROCK     => 1
    case PAPER    => 2
    case SCISSORS => 3
    case LOSE     => 0
    case DRAW     => 3
    case WIN      => 6
  }

}
