package TicTacToe.Tests;
import TicTacToe.*;


public class TestPlayer extends Player {
  private int moveIdx = 0;
  private int moves[] = new int[5];

  public TestPlayer(char symbol, String name, int moves[]) {
    super(symbol, name);
    this.moves = moves;
  }

  public int getMove(Board board, Player opponent) {
    return moves[moveIdx++];
  }
}