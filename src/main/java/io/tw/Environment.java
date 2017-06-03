package io.tw;

import java.util.Set;

public class Environment {

  private final int maxX;
  private final int maxY;

  private final Cells cells;

  public Environment(int maxX, int maxY, Set<Position> initialStates) {
    this(maxX, maxY, new Cells(initialStates));
  }

  public Environment(int maxX, int maxY, Cells cells) {
    this.maxX = maxX;
    this.maxY = maxY;
    this.cells = cells;
  }

  public boolean isValidPosition(Position position) {
    return position.x <= maxX && position.y <= maxY &&
      position.x >= 1 && position.y >= 1;
  }

}
