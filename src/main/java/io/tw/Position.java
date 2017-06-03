package io.tw;

public class Position {

  public final int x;
  public final int y;

  private Position(int x, int y) {
    this.x = x;
    this.y = y;
  }

  public static Position of(int x, int y) {
    return new Position(x, y);
  }

}
