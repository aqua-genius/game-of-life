package io.tw;

import java.util.function.Predicate;
import java.util.stream.IntStream;
import java.util.stream.Stream;

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

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof Position)) return false;
    Position position = (Position) o;
    return x == position.x && y == position.y;
  }

  @Override
  public int hashCode() {
    return 31 * x + y;
  }

  Stream<Position> getMaybeAroundPositions() {
    return IntStream.range(x - 1, x + 2).boxed().flatMap(x ->
      IntStream.range(y - 1, y + 2).mapToObj(y ->
        of(x, y)
      )
    ).filter(p -> !this.equals(p));
  }

  public Stream<Position> getAroundPositions(Predicate<Position> predicate) {
    return getMaybeAroundPositions().filter(predicate);
  }

  @Override
  public String toString() {
    return "Position(" +
      "x = " + x +
      ", y = " + y +
      ')';
  }

}
