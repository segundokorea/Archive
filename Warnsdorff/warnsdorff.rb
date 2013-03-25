require './board'
Infinity = 1.0/0


class WarnsdorffMethod
  attr_reader :size, :board, :path

  def initialize size, variation=:classic, max=Infinity, n=0
    begin
      board, path = randomish_tour Board.new(size, size, variation) ; n += 1
    end until board.full? or n >= max
    @board, @path = board, path if board.full?
  end

protected

  def randomish_tour board, path=[], coord=nil, n=1
    next_moves = best_moves_from(coord, board, path)
    return [board, path] if next_moves.empty?
    next_move = next_moves.shuffle.first
    next_board, next_path = simulate_move(next_move, board, path, n)
    randomish_tour(next_board, next_path, next_move, n + 1)
  end

  def simulate_move coord, board, path, n=1
    new_board = board.dup.set coord, n
    new_path  = path.dup << coord
    return [new_board, new_path]
  end

  def best_moves_from coord, board, path
    return board.open_squares if coord.nil?
    weighted_moves = board.moves_from(coord).map do |rc|
      [rc, board.moves_from(rc).length]
    end
    return [] if weighted_moves.empty?
    min_weight = weighted_moves.map { |m,w| w }.min
    weighted_moves.keep_if { |m,w| w == min_weight }.map(&:first)
  end
end
