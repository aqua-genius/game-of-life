package io.tw;

import java.util.Set;

class Cells {

  private final Set<Position> aliveCells;

  Cells(Set<Position> aliveCells) {
    this.aliveCells = aliveCells;
  }

  boolean isAliveCell(Position position) {
    return aliveCells.contains(position);
  }

  Set<Position> getAliveCells() {
    return aliveCells;
  }

}
