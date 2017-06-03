package io.tw;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Environment {

  public static final char CELL_ALIVE = '*';
  public static final char CELL_DEAD = ' ';

  private final int maxX;
  private final int maxY;

  private Cells cells;

  public static Environment create(int maxX, int maxY, Position[] positions) {
    final Set<Position> positionsAsSet = new HashSet<>();
    positionsAsSet.addAll(Arrays.asList(positions));
    return new Environment(maxX, maxY, new Cells(positionsAsSet));
  }

  public Environment(int maxX, int maxY, Cells cells) {
    this.maxX = maxX;
    this.maxY = maxY;
    this.cells = cells;
  }

  boolean isValidPosition(Position position) {
    return position.x <= maxX && position.y <= maxY &&
      position.x >= 1 && position.y >= 1;
  }

  public boolean calcNextCellState(Position position) {
    final Stream<Position> aroundPositions = position.getAroundPositions(this::isValidPosition);
    final int aliveCounts = (int) aroundPositions.filter(cells::isAliveCell).count();

    switch (aliveCounts) {
      case 3:
        return true;
      case 2:
        return cells.isAliveCell(position);
      default:
        return false;
    }
  }

  Stream<Position> allPositionsInEnvironment() {
    return Position.createPositionsWithRange(1, maxX, 1, maxY);
  }

  public Cells calcNextCells() {
    final Set<Position> nextAliveCellPositions =
      allPositionsInEnvironment().
        filter(this::calcNextCellState).
        collect(Collectors.toSet());

    return new Cells(nextAliveCellPositions);
  }

  public void mutate() {
    this.cells = calcNextCells();
  }

  public char[][] createScreenMatrix() {
    final char[][] screen = new char[maxY][maxX];
    for (char[] line : screen) {
      Arrays.fill(line, CELL_DEAD);
    }
    for (Position position : cells.getAliveCells()) {
      screen[position.y - 1][position.x - 1] = CELL_ALIVE;
    }
    return screen;
  }

  public String show() {
    List<String> lines = new LinkedList<>();
    final char[][] screen = createScreenMatrix();
    for (char[] chars : screen) {
      lines.add(new String(chars));
    }
    return String.join("\n", lines);
  }

}
