package io.tw;

import io.tw.util.IO;

import java.io.IOException;
import java.util.List;

public class App {

  public static void main(String[] args) throws InterruptedException, IOException {
    if (args.length < 2) {
      System.out.println("Usage: run $filepath $interval");
      return;
    }

    final String initialStatesFilePath = args[0];

    final int interval = Integer.parseInt(args[1]);

    final boolean singleRun = args.length > 2 && args[2].equals("--single-run");

    final List<String> input = IO.readFile(initialStatesFilePath);

    final int x = parseX(input.get(0));
    final int y = parseY(input.get(0));

    final Position[] initialPositions = IO.scanPositions(input.subList(1, input.size()));

    final Environment environment = Environment.create(
      x, y,
      initialPositions
    );

    while (true) {
      final String lines = environment.show();

      System.out.println("\u001B[2J\u001B[3J\u001B[H");
      System.out.println(lines);

      Thread.sleep(interval);

      environment.mutate();

      if (singleRun) return;
    }
  }

  static int parseX(String line) {
    return Integer.parseInt(line.split(",")[0].trim());
  }

  static int parseY(String line) {
    return Integer.parseInt(line.split(",")[1].trim());
  }

}
