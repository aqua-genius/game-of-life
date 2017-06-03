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

  public void testGetMaybeAroundPositions() throws Exception {
    final int x = RandomUtils.randomInt();
    final int y = RandomUtils.randomInt();
    final Position position = Position.of(x, y);
    final Stream<Position> positions = position.getMaybeAroundPositions();
    final Set<Position> positionsAsSet = positions.collect(Collectors.toSet());

    assertEquals(positionsAsSet.size(), 8);
    for (Position maybeAroundPosition : positionsAsSet) {
      assertTrue(Math.abs(maybeAroundPosition.x - position.x) <= 1);
      assertTrue(Math.abs(maybeAroundPosition.y - position.y) <= 1);
    }
  }

}
