package io.tw;

import io.tw.util.RandomUtils;
import junit.framework.TestCase;

import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class PositionTest extends TestCase {

  public void testCreatePosition() {
    final int x = RandomUtils.randomInt();
    final int y = RandomUtils.randomInt();
    final Position position = Position.of(x, y);
    assertEquals(position.x, x);
    assertEquals(position.y, y);
  }

  public void testGetAroundPositions() throws Exception {
    final int x = RandomUtils.randomInt();
    final int y = RandomUtils.randomInt();
    final Position position = Position.of(x, y);
    final Stream<Position> positions = position.getAroundPositions(p -> true);
    final Set<Position> positionsAsSet = positions.collect(Collectors.toSet());

    assertEquals(positionsAsSet.size(), 8);
    for (Position maybeAroundPosition : positionsAsSet) {
      assertTrue(Math.abs(maybeAroundPosition.x - position.x) <= 1);
      assertTrue(Math.abs(maybeAroundPosition.y - position.y) <= 1);
    }
  }

  public void testToString() {
    final Position position = Position.of(1, 2);
    assertEquals("Position(x = 1, y = 2)", position.toString());
  }

}
