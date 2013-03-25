require './board'
require './ai'
require './player'


module Game
  def new_player mark
    raise ArgumentError, 'Mark should be X or O' unless [:x, :o].include? mark
    print "#{mark.to_square}'s name: "
    name = gets.chomp
    if name =~ /^AI/
      return AI.new mark, name
    else
      return Player.new mark, name
    end
  end

  def new_board ; Board.new ; end

  def play board, player, opponent
    # Go ahead an return if the board is solved
    puts ; puts board
    winner = winner_for board, player, opponent
    return winner if winner
    return nil if board.full?

    # You can always infer the last move...
    if board.one_left?
      puts "Making last move for #{player}..."
      board.set board.open_moves.first, player.mark
      return play board, opponent, player
    end

    # Keep playing if you can
    puts "#{player}'s turn!"
    index = get_move_for board, player, opponent
    board.set index, player.mark
    return play board, opponent, player
  end

  def winner_for board, player, opponent
    return nil unless board.winner
    return board.winner == player.mark ? player : opponent
  end

  def get_move_for board, player, opponent
    print "Index #{board.open_moves.inspect}: "
    index = player.get_move board, opponent
    if index < Board::min or index > Board::max
      puts "Sorry, that's out of bounds..."
      return get_move_for board, player, opponent
    elsif board.moves_made.include? index
      puts "Sorry, that square is marked..."
      return get_move_for board, player, opponent
    end
    return index
  end
end