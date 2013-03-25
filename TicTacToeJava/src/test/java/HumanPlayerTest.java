import TicTacToe.*;
import junit.framework.TestCase;
import java.io.*;


class MoveStream extends InputStream {
  private int idx = 0;
  private char stream[] = { '\0', '\n' };

  public MoveStream(char symbol) {
    super();
    stream[0] = symbol;
  }

  public int read() {
    return stream[idx++ % 2];
  }
}


public class HumanPlayerTest extends TestCase {
  private HumanPlayer p;
  private HumanPlayer opponent;
  private Board emptyBoard;

  protected void setUp() throws Exception {
    p          = new HumanPlayer('X', "Human", new BufferedReader(new InputStreamReader(new MoveStream('0'))));
    opponent   = new HumanPlayer('O', "Anonymous", new BufferedReader(new InputStreamReader(new MoveStream('1'))));
    emptyBoard = new Board();
  }

  protected void tearDown() throws Exception {
    p          = null;
    opponent   = null;
    emptyBoard = null;
  }

  public void testZeroMove() {
    assertEquals(0, p.getMove(emptyBoard, opponent));
  }
}
