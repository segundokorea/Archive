# @       for player
# o       for crates
# #       for walls
# <space> for open floor
# .       for storage
# *       for crate on storage
# +       for player on storage
#
# Walls are impassable. The player
# can push a crate as long as there
# is no wall or crate ahead.
class Level
  attr_reader :tiles, :width, :height

  # When creating a level from scratch, leave
  # off the height and width arguments so the
  # raw tile data can be normalized
  def initialize tiles=nil, height=nil, width=nil
    @tiles, @height, @width = tiles, height, width
    if height.nil? or width.nil?
      @tiles, @width, @height = even_up tiles
    end
  end

  # Tidy up a raw string a tiles
  def even_up tiles
    maxl = tiles.lines.map(&:length).max
    lines = tiles.lines.map do |line|
      line = line.chomp
      diff = maxl - line.length
      line = line + (' ' * diff)
      line.split('')
    end
    return lines, maxl, lines.length
  end

  # Recover the tile string
  def to_s ; tiles.map(&:join).join("\n") end

  # Retrieve the character at a given coordinate
  def at coord
    r, c = coord
    return nil if r < 0 or r >= height
    return nil if c < 0 or c >= width
    return tiles[r][c]
  end

  # Set a character at the given coordinate
  def set coord, char
    char = '@' if player? char and not storage_at? coord
    char = '+' if player? char and storage_at? coord
    char = 'o' if crate? char and not storage_at? coord
    char = '*' if crate? char and storage_at? coord
    r, c = coord
    new_tiles = tiles.dup
    new_tiles[r][c] = char
    Level.new new_tiles, height, width
  end

  def set_player coord ; set coord, '@' end
  def set_crate coord ; set coord, 'o' end
  def storage_at? coord ; %w( . * + ).include? at(coord) end
  def crate_at? coord ; crate? at(coord) end
  def player? char ; %w( @ + ).include? char end
  def crate? char ; %w( o * ).include? char end

  # Move the player in the given direction, pushing
  # any crates out of the way first
  def move_player d
    here = coord_toward d
    level = unset player_coord
    if crate_at? here
      yonder = coord_toward(d, here)
      level  = level.unset yonder
      level  = level.set_crate yonder
    end
    return level.set_player(here)
  end

  # When moving off a tile, restore the floor below,
  # which may or may not be storage
  def unset coord
    char = %w( * + . ).include?(at coord) ? '.' : ' '
    set coord, char
  end

  # True if all crates have been stored
  def complete?
    tiles.flatten.select { |c| 'o' == c }.empty?
  end

  # Find all coordinates for a given character
  def locate char
    coords = []
    tiles.each_with_index do |row, i|
      row.each_with_index do |col, j|
        coords << [i,j] if col == char
      end
    end
    return coords
  end

  # Locate the player
  def player_coord
    locate('@').first || locate('+').first
  end

  # Adjustments to coordinates for a given direction
  def adjustment_for d
    case d
    when :north ; [-1, 0]
    when :south ; [ 1, 0]
    when :east  ; [ 0, 1]
    when :west  ; [ 0,-1]
    end
  end

  # Coordinate after moving one step in given direction from start
  def coord_toward d, here=player_coord
    r, c = here
    n, m = adjustment_for d
    return [r+n, c+m]
  end

  # True if player can head in given direction
  def passable? d
    r, c = player_coord
    n, m = adjustment_for d

    # Look ahead in same direction for an impass
    impass_yonder = /[\*#o]/ =~ at([r+2*n, c+2*m])
    return case at([r+n, c+m])
    when 'o' ; not impass_yonder
    when '#' ; false
    when ' ' ; true
    when '.' ; true
    when '*' ; not impass_yonder
    end
  end
end
