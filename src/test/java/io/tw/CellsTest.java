package io.tw;

import junit.framework.TestCase;

import java.util.HashSet;
import java.util.Set;

public class CellsTest extends TestCase {

  public void testIsCellAlive() throws Exception {
    final Set<Position> initialStates = new HashSet<>();
    initialStates.add(Position.of(1, 2));
    initialStates.add(Position.of(10, 20));

    final Cells cells = new Cells(initialStates);

    assertTrue(cells.isAliveCell(Position.of(1, 2)));
    assertFalse(cells.isAliveCell(Position.of(2, 1)));
  }

}
