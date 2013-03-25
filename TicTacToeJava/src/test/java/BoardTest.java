import TicTacToe.*;
import junit.framework.TestCase;


public class BoardTest extends TestCase {
  private Board b;

  protected void setUp() throws Exception {
    b = new Board();
  }

  protected void tearDown() throws Exception {
    b = null;
  }

  public void testStringifyEmptyBoard() {
    String emptyBoard = b.toString();
    assertEquals("" +
      " 1 | 2 | 3 \n" +
      "---+---+---\n" +
      " 4 | 5 | 6 \n" +
      "---+---+---\n" +
      " 7 | 8 | 9 \n",
      emptyBoard
    );
  }

  public void testStringifyAfterCenterMove() {
    b.place('X', 5);
    String board = b.toString();
    assertEquals("" +
      " 1 | 2 | 3 \n" +
      "---+---+---\n" +
      " 4 | X | 6 \n" +
      "---+---+---\n" +
      " 7 | 8 | 9 \n",
      board
    );
  }

  public void testInspectEmptyBoard() {
    for (int i = 1; i <= 9; ++i) {
      assertEquals((""+i).charAt(0), b.inspect(i));
    }
  }

  public void testInspectAfterCenterMove() {
    b.place('X', 5);
    assertEquals('X', b.inspect(5));
  }

  public void testEmptyBoardIsEmpty() {
    assertEquals(true, b.isEmpty());
    assertEquals(9, b.openMoves().size());
  }

  public void testNonEmptyBoardIsNotEmpty() {
    b.place('X', 5);
    assertEquals(false, b.isEmpty());
    assertEquals(8, b.openMoves().size());
  }

  public void testNonEmptyBoardIsNotFull() {
    b.place('X', 5);
    assertEquals(false, b.isFull());
  }

  public void testFullBoardIsFull() {
    for (int i = 1; i <= 9; ++i) b.place('X', i);
    assertEquals(true, b.isFull());
    assertEquals(0, b.openMoves().size());
  }
}