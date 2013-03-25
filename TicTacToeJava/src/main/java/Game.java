package TicTacToe;


abstract public class Game {
  protected Board board;
  protected Player player;
  protected Player opponent;

  public Game(Player x, Player o) {
    this(new Board(), x, o);
  }

  public Game(Board b, Player x, Player o) {
    board    = b;
    player   = x;
    opponent = o;
  }

  protected boolean invalidMove(int move) {
    return move < 1 || move > 9 || !board.isOpen(move);
  }

  abstract public int getMove();

  public void completeTurn() {
    int move = getMove();
    while (invalidMove(move)) move = getMove();
    board.place(player.symbol(), move);
    Player _player = player;
    player         = opponent;
    opponent       = _player;
  }

  public boolean isDecided() {
    return board.isFull() || winner() != null;
  }

  protected Player check(int a, int b, int c) {
    boolean success = board.inspect(a) == board.inspect(b) &&
                      board.inspect(a) == board.inspect(c);
    if (!success) return null;
    return board.inspect(a) == player.symbol() ? player : opponent;
  }

  public Player winner() {
    Player p = null;
    if (p == null) p = check(1,2,3); // Rows
    if (p == null) p = check(4,5,6);
    if (p == null) p = check(7,8,9);
    if (p == null) p = check(1,4,7); // Cols
    if (p == null) p = check(2,5,8);
    if (p == null) p = check(3,6,9);
    if (p == null) p = check(1,5,9); // Diags
    if (p == null) p = check(7,5,3);
    return p;
  }

  public String toString() {
    return board.toString();
  }
}