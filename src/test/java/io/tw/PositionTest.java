package io.tw;

import io.tw.util.RandomUtils;
import junit.framework.TestCase;

public class PositionTest extends TestCase {

  public void testCreatePosition() {
    final int x = RandomUtils.randomInt();
    final int y = RandomUtils.randomInt();
    final Position position = Position.of(x, y);
    assertEquals(position.x, x);
    assertEquals(position.y, y);
  }

}
