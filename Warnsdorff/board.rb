class Board
  attr_reader :height, :width, :squares, :variation

  def initialize height, width, variation=:classic, squares=Array.new(height*width, nil)
    @height, @width, @squares, @variation = height, width, squares, variation
  end

  def to_s
    l = (height * width).to_s.length
    height.times.map do |i|
      width.times.map do |j|
        square = get [i+1, j+1]
        if square.nil? ; '   '
        else ; square.to_s.center(l+2)
        end
      end.join('|')
    end.join("\n" + ('-' * ((l+3) * width - 1)) + "\n")
  end

  def get coord ; squares[idx coord] end

  def set coord, v
    new_squares = squares.dup
    new_squares[idx coord] = v
    return Board.new(height, width, variation, new_squares)
  end

  def full? ; squares.dup.keep_if(&:nil?).empty? end

  def open_squares
    result = []
    height.times do |r|
      width.times do |c|
        coord = [r+1, c+1]
        result << coord if get(coord).nil?
      end
    end
    return result
  end

  def idx coord
    r, c = coord
    raise ArgumentError, "Invalid row #{r}" if r < 1 or r > height
    raise ArgumentError, "Invalid column #{c}" if c < 1 or c > width
    return width * (r - 1) + (c - 1)
  end

  def in_bounds? coord
    r, c = coord
    r >= 1 and r <= height and c >= 1 and c <= width
  end

  def moves_from coord
    idx coord ; r, c = coord
    if variation == :classic
      jumps = # Regular Knight's Move
        [[-1, 2], [-1, -2], [1, 2], [1, -2],
         [-2, 1], [-2, -1], [2, 1], [2, -1]]
    else
      jumps = # "Pen and Paper" variation
        [[-3, 0], [ 3, 0], [0, -3], [ 0, 3],
         [ 2, 2], [-2, 2], [2, -2], [-2,-2]]
    end

    jumps.map do |x,y| [r+y, c+x]
    end.keep_if do |d| in_bounds? d and get(d).nil?
    end
  end
end
