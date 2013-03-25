require './map'
require 'set'

class Map
  def reachable_from r, c
    tile = get(r,c)
    rows = (r-1)..(r+1)
    coords  = rows.flat_map do |i|
      cols = (c-1)..(c+1)
      cols.map do |j|
        if i < 1 or i > height ; nil
        elsif j < 1 or j > width ; nil
        else [i,j, get(i,j)]
        end
      end
    end
    coords -= [[r,c, tile]]
    coords.compact
  end

  def traversable_from r, c
    neighbors = reachable_from r, c
    neighbors.keep_if { |i,j,t| t.traversable? }
    Set.new neighbors
  end
end

class Search
  attr_reader :visited, :graph, :memo

  def initialize graph, starts='@', ends='X'
    candidates = graph.find starts
    raise ArgumentError, "No canonical start symbol" unless candidates.length == 1
    @starts = candidates.first
    candidates = graph.find ends
    raise ArgumentError, "No canonical ends symbol" unless candidates.length == 1
    @ends = candidates.first
    @graph = graph
    @solution = nil
    astar
  end

  def solution
    return @solution if @solution
    m = graph.dup
    @visited.each { |r,c,t| m.set(r, c, '#') }
    @solution = m
  end

private
  def astar starts=@starts
    @visited ||= Set.new
    @memo    ||= {}

    if starts == @ends
      last  = @memo[@ends][1]
      mlast = @memo[last]
      @visited << @ends
      return @memo[@ends] = [mlast[0], last]
    end

    sr, sc, _ = starts
    er, ec, _ = @ends

    # Determine the next tiles available
    futures = graph.traversable_from(sr, sc) - visited
    return nil if futures.empty?

    # Determine cost of visiting each tile
    cost_for = ->(tile) do
      return @memo[tile] if @memo.has_key? tile
      cr, cc, ct  = tile
      @memo[tile] = [ct.cost + (er-cr).abs + (ec-cc).abs, starts]
    end
    future = futures.reduce do |m, f|
      cost_for[m][0] >= cost_for[f][0] ? f : m
    end

    # Mark the tile as visited
    @visited << starts
    astar(future)
  end
end
