package TicTacToe;


abstract public class Player {
  private char symbol;
  private String name;

  public Player(char symbol, String name) {
    this.symbol = symbol;
    this.name   = name;
  }

  public char symbol() { return symbol; }

  public String name() { return name; }

  public String toString() { return name; }

  abstract public int getMove(Board board, Player opponent);
}