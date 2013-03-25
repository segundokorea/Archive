package TicTacToe;

import java.lang.Integer;
import java.io.*;


public class HumanPlayer extends Player {
  private BufferedReader in;
  
  public HumanPlayer(char symbol, String name) {
    this(symbol, name, new BufferedReader(new InputStreamReader(System.in)));
  }
  
  public HumanPlayer(char symbol, String name, BufferedReader in) {
    super(symbol, name);
    this.in = in;
  }

  public int getMove(Board board, Player opponent) {
    int move = -1;
    try {
      String rawMove = in.readLine();
      move = Integer.parseInt(rawMove);
    } finally { return move; }
  }
}