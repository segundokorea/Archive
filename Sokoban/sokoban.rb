require './level'

module Sokoban
  def self.play_file path
    puts "And so the journey begins..."
    maps = File.read(path).split("\n\n")
    play(maps.shift, 1)
    maps.each_with_index do |map, i|
      play(map, i+2)
    end
  end

  def self.play map, n
    l = Level.new map
    until l.complete?
      system 'clear'
      puts "Level #{n}"
      puts l
      d = ask_direction l
      l = l.move_player d
    end
  end

  def self.ask_direction l
    d = get_direction
    until not d.nil? and l.passable? d
      puts "Doesn't quite cut it..."
      d = get_direction
    end
    return d
  end

  def self.get_direction
    print "Enter a direction [w,a,s,d]: "
    raw_move = gets
    return nil if raw_move.nil?
    return {
      'w' => :north, 'a' => :west,
      's' => :south, 'd' => :east
    }[raw_move.chomp]
  end
end
