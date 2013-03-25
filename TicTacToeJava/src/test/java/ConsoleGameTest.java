import TicTacToe.*;
import junit.framework.TestCase;
import TicTacToe.Tests.TestPlayer;


public class ConsoleGameTest extends TestCase {
  private TestPlayer playerX;
  private TestPlayer playerO;
  private ConsoleGame xWinsGame;
  private TestPlayer player1;
  private TestPlayer player2;
  private ConsoleGame badMoveGame;
  private ConsoleGame drawGame;

  protected void setUp() throws Exception {
    int movesForDrawX[] = { 1, 3, 5, 6, 8 };
    int movesForDrawO[] = { 2, 4, 7, 9 };
    TestPlayer drawX    = new TestPlayer('X', "Draw X", movesForDrawX);
    TestPlayer drawO    = new TestPlayer('O', "Draw O", movesForDrawO);
    drawGame            = new ConsoleGame(drawX, drawO);

    int movesForX[] = { 1, 2, 3 };
    int movesForO[] = { 9, 8 };
    playerX         = new TestPlayer('X', "Player X", movesForX);
    playerO         = new TestPlayer('O', "Player O", movesForO);
    xWinsGame       = new ConsoleGame(playerX, playerO);

    int movesForPlayer1[] = { 1, 5, 9, 3, 5 }; 
    int movesForPlayer2[] = { 2, 5, 6, 7 };
    player1 = new TestPlayer('X', "Player 1", movesForPlayer1);
    player2 = new TestPlayer('O', "Player 2", movesForPlayer2);
    badMoveGame        = new ConsoleGame(player1, player2);
  }

  protected void tearDown() throws Exception {
    playerX   = null;
    playerO   = null;
    xWinsGame = null;
    drawGame  = null;
  }

  public void testUnplayedIsNotDecided() {
    assertEquals(false, drawGame.isDecided());
    assertEquals(null, drawGame.winner());
  }

  private void playConsoleGame(ConsoleGame g) {
    while (!g.isDecided()) g.completeTurn();
  }

  public void testDraw() {
    playConsoleGame(drawGame);
    assertEquals(null, drawGame.winner());
  }

  public void testWinner() {
    playConsoleGame(xWinsGame);
    assertEquals(playerX, xWinsGame.winner());
  }

  public void testCannotOverridePreviousMove() {
    playConsoleGame(badMoveGame);
    assertEquals(player1, badMoveGame.winner());
  }
}