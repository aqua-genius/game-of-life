package io.tw;

import junit.framework.TestCase;

import java.io.IOException;

public class AppTest extends TestCase {

  public void testMain() throws IOException, InterruptedException {
    App.main(new String[]{
      "./gosper-glider-gun.txt",
      "20",
      "--single-run"
    });
  }

  public void testParseX() {
    assertEquals(1, App.parseX("1, 2"));
  }

  public void testParseY() {
    assertEquals(2, App.parseY("1, 2"));
  }

}
