// DO NOT MODIFY FOR BASIC SUBMISSION
// scalastyle:off

package snake.game

sealed abstract class Direction {
  def opposite : Direction
}

case class East()   extends Direction  { def opposite = West()  }
case class North()  extends Direction  { def opposite = South() }
case class West()   extends Direction  { def opposite = East()  }
case class South()  extends Direction  { def opposite = North() }
