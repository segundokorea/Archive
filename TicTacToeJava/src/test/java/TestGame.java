package TicTacToe.Tests;
import TicTacToe.*;


public class TestGame extends Game {
  private int moveIdx = 0;
  private int moves[] = new int[5];

  public TestGame(Player x, Player o) {
    super(x, o);
  }

  public int getMove() {
    return player.getMove(board, opponent);
  };
}