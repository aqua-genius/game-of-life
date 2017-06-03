package io.tw;

import java.io.IOException;

public class App {

  public static void main(String[] args) throws InterruptedException, IOException {
    final Position[] initialPositions = {
      Position.of(2, 1),
      Position.of(2, 2),
      Position.of(2, 3),
    };

    final Environment environment = Environment.create(3, 3, initialPositions);

    while (true) {
      final String lines = environment.show();
      Runtime.getRuntime().exec("clear");
      System.out.println(lines);
      Thread.sleep(500);
      environment.next();
    }
  }

}
