package snake.logic

import scala.collection.mutable.Stack
import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.game.{Direction, North, South, East, West, Apple, Empty, SnakeHead, SnakeBody, GridType}
import snake.logic.SnakeLogic._
import scala.collection.immutable._

class SnakeLogic(val randomGen: RandomGenerator,
                 val nrColumns: Int,
                 val nrRows: Int) {

  def this() = this(new ScalaRandomGen(), DefaultRows, DefaultColumns)

  case class SnakeCellLocation(var xPosition: Int, var yPosition: Int)
  val head = SnakeCellLocation(2, 0)
  val snakeBody1 = SnakeCellLocation(1, 0)
  val snakeBody2 = SnakeCellLocation(0, 0)
  var snake: List[SnakeCellLocation] = List(head, snakeBody1, snakeBody2)

  var isReversedGame: Boolean = false

  val appleLocStack = new Stack[(Int, Int)]
  val snakeStack = new Stack[List[(Int, Int, Direction, Int)]] //The tuple is basically (x, y, currentDirection, cellsToGrow)

  var appleLoc: (Int, Int) = setRandomApplePosition() 
  var currentDirection, allowedDirection: Direction = East()

  def isGameOver: Boolean = {
    snake.tail.contains(snake.head)
  }
  
  def step(): Unit = {
    if (!isGameOver && !isReversedGame) {
      saveCurrentState()
      currentDirection = allowedDirection
      snakeMoveInGrid()
      setGrowthIfAppleEaten()
      checkGrowth()
    }
    else if (isReversedGame) {
      if (snakeStack.nonEmpty) goInReverse()
    }
  }

  def setReverseTime(reverse: Boolean): Unit = {
    isReversedGame = reverse
  }

  def changeDir(d: Direction): Unit = {
    if(d != currentDirection.opposite) {
      allowedDirection = d
    }
  }

  def getGridTypeAt(x: Int, y: Int): GridType = {
    for(i <- snake.indices) {
      if(i == 0 && x == snake.head.xPosition && y == snake.head.yPosition) return SnakeHead(currentDirection)
      if(i > 0 && x == snake(i).xPosition && y == snake(i).yPosition) return SnakeBody(i.toFloat/snake.length)
    }
    if((x, y) == appleLoc) Apple()
    else Empty()
  }

    def giveFreeSpaces(xPos : Int, yPos : Int) : List[(Int, Int)] = {
      if (xPos > nrColumns || yPos > nrRows) {
        if (xPos > nrColumns) {
          return giveFreeSpaces(0, yPos+1)
        }
        return List()
      }
      else {
        val cellLocation = SnakeCellLocation(xPos, yPos)
        if (!snake.contains(cellLocation)) return List((xPos,yPos)) ++ giveFreeSpaces(xPos+1,yPos)
        return giveFreeSpaces(xPos+1,yPos)
      }
    }


  def setRandomApplePosition(): (Int, Int) = {
    val gridFreeSpaces: List[(Int, Int)] = giveFreeSpaces(0,0)
    if (gridFreeSpaces.nonEmpty) {
      val randomIndex = randomGen.randomInt(gridFreeSpaces.length)
      return gridFreeSpaces(randomIndex)
    }
    (5,5)
  }

  def appleEaten(): Boolean = {
    (snake.head.xPosition, snake.head.yPosition) == appleLoc
  }

  def saveCurrentState(): Unit = {
    var snakeToPush: List[(Int, Int, Direction, Int)] = List()
    for(i <- snake.indices) {
      snakeToPush = snakeToPush :+ (snake(i).xPosition, snake(i).yPosition, currentDirection, cellsToGrow)
    }
    appleLocStack.push(appleLoc)
    snakeStack.push(snakeToPush)
  }

  def storeSnakeNextPosition(): Unit = { //Stores each snake cell's next position by looking at the current cell in front of it
    for (i <- snake.length - 1 to 1 by -1) {
      snake(i).xPosition = snake(i - 1).xPosition
      snake(i).yPosition = snake(i - 1).yPosition
    }
  }

  def snakeMoveInGrid(): Unit = {
    storeSnakeNextPosition()
    currentDirection match {
      case East() => snake.head.xPosition = if(snake.head.xPosition + 1 > nrColumns - 1) 0 else snake.head.xPosition + 1
      case North() => snake.head.yPosition = if(snake.head.yPosition - 1 < 0) nrRows - 1 else snake.head.yPosition - 1
      case West() => snake.head.xPosition = if(snake.head.xPosition - 1 < 0) nrColumns - 1 else snake.head.xPosition - 1
      case South() => snake.head.yPosition = if(snake.head.yPosition + 1 > nrRows - 1) 0 else snake.head.yPosition + 1
    }
  }

  def setGrowthIfAppleEaten(): Unit = {
    if (appleEaten()) {
      appleLoc = setRandomApplePosition()
      cellsToGrow += 3
    }
  }

  def growSnake(): Unit = {
    val newSnakeBody = snake.last.copy()
    snake = snake :+ newSnakeBody
  }

  var cellsToGrow = 0
  def checkGrowth(): Unit = {
    if (cellsToGrow != 0) {
      growSnake()
      cellsToGrow -= 1
    }
  }

  def goInReverse(): Unit = {
    var snakeAfterReverse: List[SnakeCellLocation] = List()
    val previousState: List[(Int, Int, Direction, Int)] = snakeStack.top
    for(i <- previousState.indices) {
      val (xPos, yPos, prevDirection, prevGrowth) = previousState(i)
      val cell = SnakeCellLocation(xPos, yPos)
      snakeAfterReverse = snakeAfterReverse :+ cell
      cellsToGrow = prevGrowth
      currentDirection = prevDirection
      allowedDirection = prevDirection
    }
    appleLoc = appleLocStack.top
    snake = snakeAfterReverse
    snakeStack.pop()
    appleLocStack.pop()
  }
}
/** SnakeLogic companion object */
object SnakeLogic {
  val DefaultColumns = 25
  val DefaultRows = 25
}