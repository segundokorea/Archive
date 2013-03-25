import TicTacToe.*;
import TicTacToe.Tests.TestPlayer;
import junit.framework.TestCase;


public class GameTreeTest extends TestCase {
  private TestPlayer player;
  private TestPlayer opponent;
  private Board board;

  protected void setUp() throws Exception {
    int dummyMoves[] = { -1, -1, -1, -1, -1 };
    player   = new TestPlayer('X', "Ex", dummyMoves);
    opponent = new TestPlayer('O', "Oh", dummyMoves);
    board    = new Board();
  }

  protected void tearDown() throws Exception {
    player   = null;
    opponent = null;
    board    = null;
  }

  public void testMinimaxOnEmptyBoard() {
    GameTree t = new GameTree(board, player, opponent);
    assertEquals(0, t.minimax());
  }

  public void testMinimaxOnDrawBoard() {
    board.place(player.symbol(), 1);
    board.place(player.symbol(), 3);
    board.place(player.symbol(), 5);
    board.place(player.symbol(), 6);
    board.place(player.symbol(), 8);
    board.place(opponent.symbol(), 2);
    board.place(opponent.symbol(), 4);
    board.place(opponent.symbol(), 7);
    board.place(opponent.symbol(), 9);
    GameTree t = new GameTree(board, player, opponent);
    assertEquals(0, t.minimax());
  }

  public void testMinimaxOnWinningBoard() {
    board.place(player.symbol(), 1);
    board.place(opponent.symbol(), 2);
    board.place(player.symbol(), 3);
    board.place(opponent.symbol(), 4);
    board.place(player.symbol(), 5);
    board.place(opponent.symbol(), 6);
    board.place(player.symbol(), 8);
    board.place(opponent.symbol(), 7);
    GameTree t = new GameTree(board, player, opponent);
    assertEquals(1, t.minimax());
    t = null;
  }

  public void testMinimaxOnBlock() {
    board.place(opponent.symbol(), 9);
    board.place(player.symbol(), 1);
    board.place(opponent.symbol(), 5);
    board.place(player.symbol(), 2);
    board.place(opponent.symbol(), 4);
    GameTree t = new GameTree(board, player, opponent);
    assertEquals(1, t.minimax());
    t = null;
  }

  public void testMinimaxOnBlockOpponent() {
    board.place(opponent.symbol(), 1);
    board.place(player.symbol(), 5);
    board.place(opponent.symbol(), 2);
    GameTree t = new GameTree(board, player, opponent);
    assertEquals(0, t.minimax());
    t = null;
  }
}