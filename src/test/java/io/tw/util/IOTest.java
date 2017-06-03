package io.tw.util;

import io.tw.Position;
import junit.framework.TestCase;

import java.util.Arrays;
import java.util.List;

public class IOTest extends TestCase {

  public void testScanPositions() {
    final List<String> lines = Arrays.asList(
      " * ", "* *"
    );
    final Position[] positions = IO.scanPositions(lines);
    assertEquals(3, positions.length);
    assertEquals(Position.of(2, 1), positions[0]);
    assertEquals(Position.of(1, 2), positions[1]);
    assertEquals(Position.of(3, 2), positions[2]);
  }

}
