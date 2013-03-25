package TicTacToe;

import TicTacToe.GameTree;
import java.lang.Integer;


public class ComputerPlayer extends Player {
  public ComputerPlayer(char symbol, String name) {
    super(symbol, name);
  }

  public int getMove(Board board, Player opponent) {
    if (board.isEmpty()) return 5;
    return new GameTree(board, this, opponent).bestMove();
  }
}