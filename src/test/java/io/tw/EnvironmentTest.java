package io.tw;

import junit.framework.TestCase;

public class EnvironmentTest extends TestCase {

  public void testIsValidPosition() {
    final Environment environment = Environment.create(
      9, 9,
      new Position[]{
        Position.of(1, 2),
      }
    );

    assertTrue(environment.isValidPosition(Position.of(9, 9)));
    assertFalse(environment.isValidPosition(Position.of(10, 9)));
  }

  public void testCalcNextCellStateAroundTwoAlive1() {
    final Environment environment = Environment.create(
      3, 3,
      new Position[]{
        Position.of(1, 2),
        Position.of(2, 1),
      }
    );

    boolean nextCellState = environment.calcNextCellState(Position.of(2, 2));
    assertFalse(nextCellState);
  }

  public void testCalcNextCellStateAroundTwoAlive2() {
    final Environment environment = Environment.create(
      3, 3,
      new Position[]{
        Position.of(1, 2),
        Position.of(2, 1),
        Position.of(2, 2),
      }
    );

    boolean nextCellState = environment.calcNextCellState(Position.of(2, 2));
    assertTrue(nextCellState);
  }

  public void testCalcNextCellStateAroundThreeAlive1() {
    final Environment environment = Environment.create(
      3, 3,
      new Position[]{
        Position.of(1, 1),
        Position.of(1, 2),
        Position.of(2, 1),
      }
    );

    boolean nextCellState = environment.calcNextCellState(Position.of(2, 2));
    assertTrue(nextCellState);
  }

  public void testCalcNextCellStateAroundThreeAlive2() {
    final Environment environment = Environment.create(
      3, 3,
      new Position[]{
        Position.of(1, 1),
        Position.of(1, 2),
        Position.of(2, 1),
        Position.of(2, 2)
      }
    );

    boolean nextCellState = environment.calcNextCellState(Position.of(2, 2));
    assertTrue(nextCellState);
  }

  public void testCalcNextCellStateOthers1() {
    final Environment environment = Environment.create(
      3, 3,
      new Position[]{
        Position.of(1, 1),
      }
    );

    boolean nextCellState = environment.calcNextCellState(Position.of(2, 2));
    assertFalse(nextCellState);
  }

  public void testCalcNextCellStateOthers2() {
    final Environment environment = Environment.create(
      3, 3,
      new Position[]{
        Position.of(1, 1),
        Position.of(1, 2),
        Position.of(1, 3),
        Position.of(2, 1),
        Position.of(2, 3),
      }
    );

    boolean nextCellState = environment.calcNextCellState(Position.of(2, 2));
    assertFalse(nextCellState);
  }

  public void testAllPositionInEnvironment() {
    final Environment environment = Environment.create(
      3, 3,
      new Position[]{}
    );

    assertEquals(environment.allPositionsInEnvironment().count(), 3 * 3);
  }

  public void testCalcNextCells1() {
    final Environment environment = Environment.create(
      3, 3,
      new Position[]{
        Position.of(1, 1),
        Position.of(1, 3),
        Position.of(2, 2),
        Position.of(3, 1),
        Position.of(3, 3),
      }
    );

    final Cells cells = environment.calcNextCells();
    assertEquals(cells.getAliveCells().size(), 4);
    assertTrue(cells.isAliveCell(Position.of(1, 2)));
    assertFalse(cells.isAliveCell(Position.of(1, 1)));
  }

  public void testCalcNextCells2() {
    final Environment environment = Environment.create(
      3, 3,
      new Position[]{
        Position.of(2, 1),
        Position.of(2, 3),
        Position.of(3, 1),
      }
    );

    final Cells cells = environment.calcNextCells();
    assertEquals(cells.getAliveCells().size(), 2);
    assertTrue(cells.isAliveCell(Position.of(2, 2)));
    assertFalse(cells.isAliveCell(Position.of(2, 3)));
  }

  public void testShow() {
    final Environment environment = Environment.create(
      3, 3,
      new Position[]{
        Position.of(2, 1),
        Position.of(2, 3),
        Position.of(3, 1),
      }
    );

    final String lines = environment.show();
    final String result =
      "   \n" +
        "* *\n" +
        "*  ";
    assertEquals(lines, result);
  }

}
