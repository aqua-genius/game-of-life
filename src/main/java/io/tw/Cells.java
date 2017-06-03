package io.tw;

import java.util.Set;

public class Cells {

  private final Set<Position> aliveCells;

  public Cells(Set<Position> aliveCells) {
    this.aliveCells = aliveCells;
  }

  public boolean isAliveCell(Position position) {
    return aliveCells.contains(position);
  }

  public Set<Position> getAliveCells() {
    return aliveCells;
  }

}
