require './tile'

class Map
  attr_reader :width, :height, :length, :tiles

  def initialize raw_tiles
    @raw_tiles, @height = raw_tiles, 0
    @tiles  = raw_tiles.lines.flat_map do |l|
      @height += 1
      l.chomp.chars.map(&:to_tile)
    end
    @width  = tiles.length / height
    @length = width * height
    raise ArgumentError, "Dimensions are too small" if height < 2 or width < 2
  end

  def dup ; Map.new @raw_tiles end

  def index r, c
    raise ArgumentError, "Row (#{r}) out of bounds" unless (1..height).include? r
    raise ArgumentError, "Column (#{c}) out of bounds" unless (1..width).include? c
    return width * (r-1) + (c-1)
  end

  def coords i
    j = i - 1
    c = (j % width) + 1
    r = (j / width).floor + 1
    return [r,c]
  end

  def find t
    t = t.to_tile
    (1..length).zip(tiles).select do |i, tile|
      tile.sign == t.sign
    end.map do |i, tile|
      r, c = coords i
      [r, c, tile]
    end
  end

  def get r, c ; tiles[index(r,c)] end

  def set r, c, t
    @tiles[index(r,c)] = t.to_tile
    return self
  end

  def [] i, j=nil
    return get(i,j) unless j.nil?
    return find(i)
  end

  def []= i, j, k=nil
    return set(i, j, k) unless k.nil?
    r, c = coords i
    return set(r, c, j)
  end

  def to_s
    (1..height).map do |r|
      i = index r, 1
      j = index r, width
      tiles[i..j].join
    end.join "\n"
  end
end
