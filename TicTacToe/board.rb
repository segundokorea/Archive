class Symbol
  def to_square safety=nil
    # return 'X' if self == :x
    # return 'O' if self == :o
    # return safety.to_s if safety
    return ' X' if self == :x
    return ' O' if self == :o
    return "%2d" % [safety] if safety
    return '  '
  end
end


class NilClass
  def to_square safety=nil
    # return safety.to_s if safety
    # return ' '
    return "%2d" % [safety] if safety
    return '  '
  end
end


class Board
  @@dim = 4
  @@min, @@max = 1, @@dim ** 2
  @@all_moves  = (@@min..@@max).to_a
  attr_reader :places

  def self.dim ; @@dim ; end
  def self.min ; @@min ; end
  def self.max ; @@max ; end

  def initialize places=nil
    @places = places || {}
    @@all_moves.each { |i| @places[i] = nil } unless places
    return self
  end

  def to_s
    squares = ->(i) { @places[i].to_square(i) }
    return ' ' +
      # squares[1] + ' | ' + squares[2] + ' | ' + squares[3] + " \n-------------\n " +
      # squares[4] + ' | ' + squares[5] + ' | ' + squares[6] + " \n-------------\n " +
      # squares[7] + ' | ' + squares[8] + ' | ' + squares[9] + ' '
      squares[1]  + ' | ' + squares[2]  + ' | ' + squares[3]  + ' | ' + squares[4]  + " \n-------------------\n " +
      squares[5]  + ' | ' + squares[6]  + ' | ' + squares[7]  + ' | ' + squares[8]  + " \n-------------------\n " +
      squares[9]  + ' | ' + squares[10] + ' | ' + squares[11] + ' | ' + squares[12] + " \n-------------------\n " +
      squares[13] + ' | ' + squares[14] + ' | ' + squares[15] + ' | ' + squares[16] + ' '
  end

  def winner
    # check = ->(i, j, k) do
    #   run = [get(i), get(j), get(k)]
    #   return :x if [:x, :x, :x] == run
    #   return :o if [:o, :o, :o] == run
    #   return nil
    # end
    # check[1,2,3] || check[4,5,6] || check[7,8,9] || # Rows
    # check[1,4,7] || check[2,5,8] || check[3,6,9] || # Columns
    # check[1,5,9] || check[3,5,7]                    # Diagonals
    check = ->(i, j, k, l) do
      run = [get(i), get(j), get(k), get(l)]
      return :x if [:x, :x, :x, :x] == run
      return :o if [:o, :o, :o, :o] == run
      return nil
    end
    check[ 1, 2, 3, 4] || check[ 5, 6, 7, 8] || # Rows
    check[ 9,10,11,12] || check[13,14,15,16] || #
    check[ 1, 5, 9,13] || check[ 2, 6,10,14] || # Columns
    check[ 3, 7,11,15] || check[ 4, 8,12,16] || #
    check[ 1, 6,11,16] || check[ 4, 7,10,13]    # Diagonals
  end

  def dup ; Board.new(@places.dup) ; end

  def all_moves ; @@all_moves ; end

  def moves_made ; @places.dup.keep_if { |i, mark| not mark.nil? }.keys ; end

  def open_moves ; all_moves - moves_made ; end

  def full? ; open_moves.empty? ; end

  def empty? ; open_moves.length == @@max ; end

  def one_left? ; open_moves.length == @@min ; end

  def get index
    raise ArgumentError, 'Index is out of bounds' if index < @@min or index > @@max
    @places[index]
  end

  def set index, mark
    raise ArgumentError, 'Index is out of bounds' if index < @@min or index > @@max
    raise ArgumentError, 'Mark should be X or O' unless [:x, :o].include? mark
    @places[index] = mark
    self
  end
end