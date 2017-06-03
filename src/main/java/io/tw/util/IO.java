package io.tw.util;

import io.tw.Environment;
import io.tw.Position;

import java.io.*;
import java.util.LinkedList;
import java.util.List;

public class IO {

  private static final char CELL_ALIVE_1 = '1';

  public static List<String> readFile(String filePath) throws IOException {
    File file = new File(filePath);
    List<String> lines = new LinkedList<>();
    try (
      BufferedReader reader =
        new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"))
    ) {

      while (true) {
        String line = reader.readLine();
        if (line == null) break;
        lines.add(line);
      }
      return lines;
    }
  }

  public static Position[] scanPositions(List<String> input) {
    final List<Position> positions = new LinkedList<>();

    for (int i = 0; i < input.size(); i += 1) {
      final char[] inputLine = input.get(i).toCharArray();

      for (int j = 0; j < inputLine.length; j += 1) {
        char c = inputLine[j];
        if (c == Environment.CELL_ALIVE || c == CELL_ALIVE_1) {
          positions.add(Position.of(j + 1, i + 1));
        }
      }
    }

    Position[] initialPositions = new Position[positions.size()];

    initialPositions = positions.toArray(initialPositions);

    return initialPositions;
  }

}
