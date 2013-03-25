import TicTacToe.*;
import junit.framework.TestCase;
import TicTacToe.Tests.*;


public class ComputerPlayerTest extends TestCase {
  private ComputerPlayer computer;
  private TestPlayer opponent;
  private Board board;

  protected void setUp() throws Exception {
    int testMoves[] = { -1, -1, -1, -1, -1 };
    opponent   = new TestPlayer('X', "Anonymous", testMoves);
    computer   = new ComputerPlayer('O', "Computer");
    board      = new Board();
  }

  protected void tearDown() throws Exception {
    computer = null;
    opponent = null;
    board    = null;
  }

  public void testMakesCenterMoveOnEmptyBoard() {
    assertEquals(5, computer.getMove(board, opponent));
  }
  
  public void testMakesSimpleBlock() {
    board.place(opponent.symbol(), 1);
    board.place(computer.symbol(), 5);
    board.place(opponent.symbol(), 2);
    assertEquals(3, computer.getMove(board, opponent));
  }

  public void testTakesWinOverBlock() {
    board.place(opponent.symbol(), 1);
    board.place(computer.symbol(), 7);
    board.place(opponent.symbol(), 2);
    board.place(computer.symbol(), 8);
    board.place(opponent.symbol(), 4);
    assertEquals(9, computer.getMove(board, opponent));
  }
}