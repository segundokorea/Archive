package TicTacToe;

import java.lang.Character;
import java.util.List;
import java.util.ArrayList;
import java.lang.Cloneable;


public class Board implements Cloneable {
  private char squares[] = new char[9];

  public Board() {
    for (int i = 0; i < 9; ++i) squares[i] = ("" + (i + 1)).charAt(0);
  }

  private Board(char squares[]) {
    this.squares = squares;
  }

  public Board clone() {
    return new Board(squares.clone());
  }

  public void place(char symbol, int move) {
    squares[move - 1] = symbol;
  }

  public char inspect(int move) {
    return squares[move - 1];
  }

  public boolean isOpen(int move) {
    return Character.isDigit(squares[move - 1]);
  }

  public List<Integer> openMoves() {
    List<Integer> moves = new ArrayList<Integer>();
    for (int move = 1; move <= 9; ++move) {
      if (isOpen(move)) moves.add(move);
    } return moves;
  }

  public boolean isEmpty() {
    return 9 == openMoves().size();
  }

  public boolean isFull() {
    return 0 == openMoves().size();
  }

  @Override public String toString() {
    String result = "";
    for (int i = 0; i < 3; ++i) {
      String a = String.valueOf(squares[3*i + 0]);
      String b = String.valueOf(squares[3*i + 1]);
      String c = String.valueOf(squares[3*i + 2]);
      result  += " " + a + " | " + b + " | " + c + " \n";
      if (i < 2) result += "---+---+---\n";
    } return result;
  }
}