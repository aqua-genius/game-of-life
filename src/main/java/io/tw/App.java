package io.tw;

import io.tw.util.IO;

import java.io.IOException;
import java.util.List;

public class App {

  private static final int INTERVAL = 20;

  public static void main(String[] args) throws InterruptedException, IOException {
    if (args.length < 1) {
      System.out.println("need filename");
      return;
    }

    final String filePath = args[0];
    final List<String> input = IO.readFile(filePath);

    final String[] xy = input.get(0).split(",");
    final int x = Integer.parseInt(xy[0].trim());
    final int y = Integer.parseInt(xy[1].trim());

    final Position[] initialPositions = IO.scanPositions(input.subList(1, input.size()));

    final Environment environment = Environment.create(
      x, y,
      initialPositions
    );

    while (true) {
      final String lines = environment.show();
      System.out.println("\u001B[2J\u001B[3J\u001B[H");
      System.out.println(lines);
      Thread.sleep(INTERVAL);
      environment.mutate();
    }
  }

}
