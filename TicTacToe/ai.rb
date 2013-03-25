require './player'
require './board'


class Fixnum
  def negate ; -1 * self ; end
end


class Board
  def futures_for player
    return self.open_moves.map do |move|
      [move, self.dup.set(move, player.mark)]
    end
  end
end


class Player
protected
  def minimax board, opponent
    score = case board.winner
      when nil  ; 0 # Draw
      when mark ; 1 # Win
      else       -1 # Lose
    end
    return score if board.full? or score != 0
    return board.futures_for(self).map { |m, b| opponent.minimax(b, self).negate }.max
  end

  def alpha_beta board, player, opponent, alpha, beta, depth
    score = case board.winner
      when nil  ; 0
      when mark ; 1
      else       -1
    end
    return score if board.full? or depth <= 0 or score != 0

    if self == player # Maximizing
      board.futures_for(player).map do |m, b|
        score = alpha_beta(b, opponent, player, alpha, beta, depth-1)
        alpha = [score, alpha].max
        return alpha if alpha >= beta
      end
      return alpha
    else # Minimizing
      board.futures_for(player).map do |m, b|
        score = alpha_beta(b, opponent, player, alpha, beta, depth-1)
        beta  = [score, beta].min
        return beta if alpha >= beta
      end
      return beta
    end
  end
end


Infinity = 1.0/0

class AI < Player
  def get_move board, opponent
    lookahead = Board::dim < 4 ? Infinity : (Board::dim * 1.5).floor - 1
    scores    = board.futures_for(self).map { |m, b| [m, opponent.alpha_beta(b, opponent, self, -Infinity, Infinity, lookahead).negate] }
    # scores    = board.futures_for(self).map { |m, b| [m, opponent.minimax(b, self).negate] }
    best_move = scores.inject { |memo, score| memo[1] > score[1] ? memo : score }[0]
    puts best_move
    return best_move
  end
end