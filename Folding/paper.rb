class Array
  def halve
    l = self.length / 2
    [take(l), drop(l)]
  end
end


class Paper
  attr_reader :nrows, :ncols, :rows, :cols

  def initialize nrows, ncols, rows=nil, cols=nil
    raise ArgumentError unless Math.log(nrows, 2) % 1 == 0
    raise ArgumentError unless Math.log(ncols, 2) % 1 == 0
    @nrows, @ncols = nrows, ncols
    @rows = rows || (1..nrows).map do |r|
      (1..ncols).flat_map do |c|
        c + ncols * (r - 1)
      end
    end
    @cols = cols || @rows.transpose
  end

  def to_s
    l = (nrows * ncols).to_s.length
    grid = (1..nrows).map do |r|
      ' ' + (1..ncols).map do |c|
        (c + ncols * (r - 1)).to_s.center(l)
      end.join(' | ') + ' '
    end
    grid.join("\n" + ('-' * grid.last.length) + "\n")
  end

  def fold instructions
    paper = self
    until instructions.empty?
      nrows, ncols = paper.nrows, paper.ncols
      rows, cols   = paper.rows, paper.cols
      case instructions.shift
      when 'T'
        paper = paper.reflectT nrows, ncols, rows, cols
      when 'R'
        paper = paper.reflectR nrows, ncols, rows, cols
      when 'B'
        paper = paper.reflectB nrows, ncols, rows, cols
      when 'L'
        paper = paper.reflectL nrows, ncols, rows, cols
      end
    end
    return paper.rows.flatten
  end

protected

  def reflectT nr, nc, rs, cs
    raise ArgumentError if nr < 2
    rows = cs.map do |c|
      top, bottom = c.halve
      top.reverse.zip(bottom)
    end.transpose
    Paper.new(nrows / 2, ncols, rows)
  end

  def reflectR nr, nc, rs, cs
    raise ArgumentError if nc < 2
    rows = rs.map do |r|
      left, right = r.halve
      right.reverse.zip(left)
    end
    Paper.new(nrows, ncols / 2, rows)
  end

  def reflectB nr, nc, rs, cs
    raise ArgumentError if nr < 2
    rows = cs.map do |c|
      top, bottom = c.halve
      bottom = bottom.map(&:reverse) rescue bottom
      bottom.reverse.zip(top)
    end.transpose
    Paper.new(nrows / 2, ncols, rows)
  end

  def reflectL nr, nc, rs, cs
    raise ArgumentError if nc < 2
    rows = rs.map do |r|
      left, right = r.halve
      left = left.map(&:reverse) rescue left
      left.reverse.zip(right)
    end
    Paper.new(nrows, ncols / 2, rows)
  end
end
