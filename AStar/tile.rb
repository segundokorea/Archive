class Tile
  attr_reader :sign, :description
  def to_tile ; self end
  def traversable? ; false end
  def to_s ; sign end
end

TERRAIN = {
  'Visited'  => { :sign => '#' },
  'Begins'   => { :sign => '@', :cost => 0 },
  'Ends'     => { :sign => 'X', :cost => 0 },
  'Water'    => { :sign => '~' },
  'Flatland' => { :sign => '.', :cost => 1 },
  'Forest'   => { :sign => '*', :cost => 2 },
  'Mountain' => { :sign => '^', :cost => 3 }
}.each do |name, fields|
  klass = Class.new Tile
  fields.each do |fname, fval|
    klass.send :define_method, fname, lambda { fval }
    if fname == :cost
      klass.send :define_method, 'traversable?', lambda { true }
    end
  end
  Kernel.const_set name, klass
end

class String
  def to_tile
    candidates = TERRAIN.select { |name, fields| fields[:sign] == self }
    return nil if candidates.empty?
    terrain_klass = Kernel.const_get candidates.first.first.to_sym
    terrain_klass.new
  end
end
