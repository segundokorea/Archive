package TicTacToe;

import TicTacToe.Game;
import java.lang.Integer;
import java.util.List;
import java.util.ArrayList;


class GameTreeGame extends Game {
  public GameTreeGame(Board b, Player x, Player o) { super(b, x, o); }
  public int getMove() { return -1; }
}


public class GameTree {
  private int move;
  private Board board;
  private Player player;
  private Player opponent;
  private GameTreeGame game;

  private List<GameTree> children() {
    List<GameTree> children = new ArrayList<GameTree>();
    if (!isTerminal()) {
      for (Integer nextMove : board.openMoves()) {
        Board newBoard = board.clone();
        newBoard.place(player.symbol(), nextMove);
        children.add(new GameTree(nextMove, newBoard, opponent, player));
      }
    } return children;
  }

  public boolean isTerminal() {
    return game.isDecided();
  }

  private int score() {
    Player winner  = game.winner();
    return winner == null ? 0 : (winner == player ? 1 : -1);
  }

  private int minimax(int depth) {
    if (isTerminal() || depth <= 0) return score();
    int alpha = Integer.MIN_VALUE;
    for (GameTree t : children()) {
      int candidate = -1 * t.minimax(depth - 1);
      alpha = candidate > alpha ? candidate : alpha;
    } return alpha;
  }

  public int minimax() {
    return minimax(9);
  }

  public int bestMove() {
    int bestScore = Integer.MIN_VALUE;
    int bestMove  = -1;
    for (GameTree t : children()) {
      int score = -1 * t.minimax();
      if (score >= bestScore) {
        bestScore = score;
        bestMove  = t.move;
      }
    } return bestMove;
  }

  public GameTree(Board board, Player player, Player opponent) {
    this(-1, board, player, opponent);
  }

  public GameTree(int move, Board board, Player player, Player opponent) {
    this.move     = move;
    this.board    = board;
    this.player   = player;
    this.opponent = opponent;
    this.game     = new GameTreeGame(board, player, opponent);
  }
}