object IteratorHelpers {
  def randomOr[A](firstProb: Double, it1: Iterator[A], it2: Iterator[A]): Iterator[A] = {
    if (math.random < firstProb) {
      it1 ++ it2
    } else {
      it2 ++ it1
    }
  }

  // gets the first solution from the given call-by-name iterator indefinitely
  // loops infinitely if the given iterator will never produce solutions
  def repeat[A](it: => Iterator[A]): Iterator[A] = {
    new Iterator[A] {
      def hasNext: Boolean = true
      def next: A = {
        var curIt: Iterator[A] = it
        while (!curIt.hasNext) {
          curIt = it
        }
        curIt.next
      }
    }
  }
}
import IteratorHelpers._

class Display(val width: Int, val height: Int) {
  private val arr: Array[Array[Char]] = Array.ofDim(width, height)
  clear()

  def clear(): Unit = {
    0.until(width).foreach(row =>
      0.until(height).foreach(col =>
        write(row, col, ' ')))
  }

  def write(row: Int, col: Int, what: Char): Unit = {
    arr(row)(col) = what
  }

  def joinRoomsHorizontal(): Unit = {
    // any instance of _xx_ can be replaced with ____

    0.until(height).foreach(row =>
      0.until(width - 3).foreach(col => {
        if (arr(col)(row) == '_' &&
          arr(col + 1)(row) == 'x' &&
          arr(col + 2)(row) == 'x' &&
          arr(col + 3)(row) == '_') {
          write(col + 1, row, '_')
          write(col + 2, row, '_')
        }
      }))
  }

  def joinRoomsVertical(): Unit = {
    0.until(height - 3).foreach(row =>
      0.until(width).foreach(col => {
        if (arr(col)(row) == '_' &&
          arr(col)(row + 1) == 'x' &&
          arr(col)(row + 2) == 'x' &&
          arr(col)(row + 3) == '_') {
          write(col, row + 1, '_')
          write(col, row + 2, '_')
        }
      }))
  }

  def joinRooms(): Unit = {
    joinRoomsHorizontal()
    joinRoomsVertical()
  }

  def print(): Unit = {
    joinRooms()
    0.until(width).foreach(row => {
      0.until(height).foreach(col => {
        Console.print(arr(row)(col))
      })
      println()
    })
  }
}

// the smallest permitted partition needs to be able to admit
// at least a 3x3 rectangle.  Anything smaller leads to "rooms"
// that are nothing but walls
//
// xxx
// x x
// xxx
//
//
// We can also intentionally make this larger, to avoid tiny rooms
object Rectangle {
  val MIN_ROOM_SIZE = 7
}
import Rectangle.MIN_ROOM_SIZE

case class Rectangle(xStart: Int, xEnd: Int, yStart: Int, yEnd: Int) {
  assert(xStart + MIN_ROOM_SIZE <= xEnd)
  assert(yStart + MIN_ROOM_SIZE <= yEnd)

  def writeTo(display: Display): Unit = {
    // write four lines for the walls:
    // xStart to xEnd, at yStart
    // xStart to xEnd, at yEnd
    // yStart to yEnd, at xStart
    // yStart to yEnd, at xEnd
    xStart.to(xEnd).foreach(x => {
      display.write(x, yStart, 'x')
      display.write(x, yEnd, 'x')
    })
    yStart.to(yEnd).foreach(y => {
      display.write(xStart, y, 'x')
      display.write(xEnd, y, 'x')
    })

    // now write the traversable space inside
    (xStart + 1).until(xEnd).foreach(x =>
      (yStart + 1).until(yEnd).foreach(y =>
        display.write(x, y, '_')))
  }

  def randomizedRange(starting: Int, ending: Int): Iterator[Int] = {
    scala.util.Random.shuffle(starting.to(ending).toSeq).iterator
  }

  // gives back a rectangle that is either this one, or contained within
  // this one
  def subrec: Iterator[Rectangle] = {
    // need newXStart, newXEnd, newYStart, and newYEnd, such that:
    // xStart <= newXStart <= newXEnd <= xEnd,
    // newXStart + MIN_ROOM_SIZE <= newXEnd,
    // yStart <= newYStart <= newYEnd <= yEnd,
    // newYStart + MIN_ROOM_SIZE <= newYEnd
    for {
      newXStart <- randomizedRange(xStart, xEnd)
      newXEnd <- randomizedRange(newXStart, xEnd)
      if newXStart + MIN_ROOM_SIZE <= newXEnd
      newYStart <- randomizedRange(yStart, yEnd)
      newYEnd <- randomizedRange(newYStart, yEnd)
      if newYStart + MIN_ROOM_SIZE <= newYEnd
    } yield Rectangle(newXStart, newXEnd, newYStart, newYEnd)
  }

  // gives back two rectangles that both satisfy minimums, and fully
  // occupy this space.  The rectangles are split over the X axis,
  // meaning they will both retain the same height
  def splitX: Iterator[(Rectangle, Rectangle)] = {
    for {
      splitPoint <- randomizedRange(xStart, xEnd)
      leftXEnd = splitPoint
      rightXStart = splitPoint + 1
      if xStart + MIN_ROOM_SIZE <= leftXEnd && rightXStart + MIN_ROOM_SIZE <= xEnd
    } yield (Rectangle(xStart, leftXEnd, yStart, yEnd),
             Rectangle(rightXStart, xEnd, yStart, yEnd))
  }

  def splitY: Iterator[(Rectangle, Rectangle)] = {
    for {
      splitPoint <- randomizedRange(yStart, yEnd)
      bottomYEnd = splitPoint
      topYStart = splitPoint + 1
      if yStart + MIN_ROOM_SIZE <= bottomYEnd && topYStart + MIN_ROOM_SIZE <= yEnd
    } yield (Rectangle(xStart, xEnd, yStart, bottomYEnd),
             Rectangle(xStart, xEnd, topYStart, yEnd))
  }

  def split: Iterator[(Rectangle, Rectangle)] = {
    randomOr(0.5, splitX, splitY)
  }
}

object BSP {
  val WIDTH = 80
  val HEIGHT = 50

  // So I can see MANY possible optimizations here, based on size constraints
  // working out or not.  A lot of this feels like it should be thrown to a constraint
  // solver (I'm brute-forcing integers all over...).
  //
  // Nondeterministic choices:
  // 1.) Build a room in this space which fits in this space
  // 2.) split the space, and recursively apply
  def partition(space: Rectangle): Iterator[Set[Rectangle]] = {
    val buildRoom = space.subrec.map(rec => Set(rec))
    val recursivePartition = for {
      (first, second) <- space.split
      firstRooms <- partition(first)
      secondRooms <- partition(second)
    } yield firstRooms ++ secondRooms
    randomOr(0.9, recursivePartition, buildRoom)
  }

  def main(args: Array[String]): Unit = {
    val display = new Display(WIDTH, HEIGHT)
    repeat(partition(Rectangle(0, WIDTH - 1, 0, HEIGHT - 1))).foreach(rooms => {
      display.clear()
      rooms.foreach(_.writeTo(display))
      display.print()
      println("--------------------------------------------")
    })
  }
}
