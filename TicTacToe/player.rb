class Player
  attr_reader :mark, :name

  def initialize mark, name
    @mark, @name = mark, name
  end

  def to_s ; name ; end

  # Subclasses should reimplement this method.
  # Given the current board and opposing player,
  # decide which board index you'd like to mark.
  def get_move board, opponent
    gets.chomp.to_i
  end
end