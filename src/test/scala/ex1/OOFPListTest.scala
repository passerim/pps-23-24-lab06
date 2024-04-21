package ex1

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.{BeforeEach, Test}

class OOFPListTest:
  var reference: List[Int] = List()

  @BeforeEach def init(): Unit =
    reference = List(1, 2, 3, 4)

  @Test def zipWithValueTest(): Unit =
    assertEquals(List((1, 10), (2, 10), (3, 10), (4, 10)), reference.zipWithValue(10))

  @Test def lengthTest(): Unit =
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), reference.zipWithIndex)

  @Test def zipWithIndexTest(): Unit = assertEquals(4, reference.length)

  @Test def partitionTest(): Unit =
    assertEquals((List(2, 4), List(1, 3)), reference.partition(_ % 2 == 0))

  @Test def spanTest(): Unit =
    assertEquals((List(1), List(2, 3, 4)), reference.span(_ % 2 != 0))
    assertEquals((List(1, 2), List(3, 4)), reference.span(_ < 3))

  @Test def takeRightTest(): Unit =
    assertEquals(List(2, 3, 4), reference.takeRight(3))

  @Test def collectTest(): Unit =
    assertEquals(List(3, 5), reference.collect({ case x if x % 2 == 0 => x + 1 }))
