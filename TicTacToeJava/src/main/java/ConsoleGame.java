package TicTacToe;

import java.lang.Integer;
import java.lang.Character;
import java.io.*;


public class ConsoleGame extends Game {
  private static BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
  private static PrintStream out   = System.out;
  private static PrintStream err   = System.err;

  public ConsoleGame(Player x, Player o) {
    super(x, o);
  }

  public int getMove() {
    out.println(player.name() + ", enter your move [1-9]: ");
    return player.getMove(board, opponent);
  }

  private static void printBoard(Game g) {
    out.print(g.toString());
  }

  private static Player getPlayer(char symbol) {
    out.print("Name for " + Character.toString(symbol) + ": ");
    String name = Character.toString(symbol);
    try {
      name = in.readLine().replace("\n","").replace("\r","");
    } finally {
      if (name.matches("^AI.*"))
        return new ComputerPlayer(symbol, name);
      return new HumanPlayer(symbol, name, in);
    }
  }

  public static void main(String[] args) {
    out.println("Tic-Tac-Toe");
    Player x = getPlayer('X');
    Player o = getPlayer('O');

    ConsoleGame g = new ConsoleGame(x, o);
    while (!g.isDecided()) {
      printBoard(g);
      g.completeTurn();
      out.println();
    } printBoard(g);

    if (g.winner() != null) out.println(g.winner().name() + " wins!");
    else out.println("It's a draw!");
  }
}