class Display(width: Int, height: Int) {
  private val arr: Array[Array[Char]] = Array.ofDim(width, height)
  clear()

  def clear(): Unit = {
    0.until(arr.length).foreach(row =>
      0.until(arr(row).length).foreach(col =>
        write(row, col, ' ')))
  }

  def write(row: Int, col: Int, what: Char): Unit = {
    arr(row)(col) = what
  }

  def print(): Unit = {
    0.until(arr.length).foreach(row => {
      0.until(arr(row).length).foreach(col => {
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
case class Rectangle(xStart: Int, xEnd: Int, yStart: Int, yEnd: Int) {
  assert(xStart + 3 <= xEnd)
  assert(yStart + 3 <= yEnd)

  def writeTo(display: Display): Unit = {
    // write four lines:
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
  }

  // gives back a rectangle that is either this one, or contained within
  // this one
  def subrec: Iterator[Rectangle] = {
    // need newXStart, newXEnd, newYStart, and newYEnd, such that:
    // xStart <= newXStart <= newXEnd <= xEnd,
    // newXStart + 3 <= newXEnd,
    // yStart <= newYStart <= newYEnd <= yEnd,
    // newYStart + 3 <= newYEnd
    for {
      newXStart <- xStart.to(xEnd).iterator
      newXEnd <- newXStart.to(xEnd).iterator
      if newXStart + 3 <= newXEnd
      newYStart <- yStart.to(yEnd).iterator
      newYEnd <- newYStart.to(yEnd).iterator
      if newYStart + 3 <= newYEnd
    } yield Rectangle(newXStart, newXEnd, newYStart, newYEnd)
  }

  // gives back two rectangles that both satisfy minimums, and fully
  // occupy this space.  The rectangles are split over the X axis,
  // meaning they will both retain the same height
  def splitX: Iterator[(Rectangle, Rectangle)] = {
    for {
      splitPoint <- xStart.to(xEnd).iterator
      leftXEnd = splitPoint
      rightXStart = splitPoint + 1
      if xStart + 3 <= leftXEnd && rightXStart + 3 <= xEnd
    } yield (Rectangle(xStart, leftXEnd, yStart, yEnd),
             Rectangle(rightXStart, xEnd, yStart, yEnd))
  }

  def splitY: Iterator[(Rectangle, Rectangle)] = {
    for {
      splitPoint <- yStart.to(yEnd).iterator
      bottomYEnd = splitPoint
      topYStart = splitPoint + 1
      if yStart + 3 <= bottomYEnd && topYStart + 3 <= yEnd
    } yield (Rectangle(xStart, xEnd, yStart, bottomYEnd),
             Rectangle(xStart, xEnd, topYStart, yEnd))
  }

  def split: Iterator[(Rectangle, Rectangle)] = {
    splitX ++ splitY
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

  def randomOr[A](firstProb: Double, it1: Iterator[A], it2: Iterator[A]): Iterator[A] = {
    if (math.random < firstProb) {
      it1 ++ it2
    } else {
      it2 ++ it1
    }
  }

  def partition(space: Rectangle): Iterator[Set[Rectangle]] = {
    val buildRoom = space.subrec.map(rec => Set(rec))
    val recursivePartition = for {
      (first, second) <- space.split
      firstRooms <- partition(first)
      secondRooms <- partition(second)
    } yield firstRooms ++ secondRooms
    randomOr(0.9, recursivePartition, buildRoom)
  }

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
