class Automaton
  attr_reader :rule, :steps, :start, :char, :output

  def initialize num=5, steps=5, start='1', char='|'
    @rule, @steps, @start, @char = num_to_rule(num), steps, start.split(''), char
    @output = exec
  end

  def to_s
    rows = output.map(&:join)
    maxl = rows.last.length
    rows.map do |r|
      r.gsub('1',@char).gsub('0',' ').center(maxl)
    end.join("\n")
  end

  def num_to_rule n
    bs = ("%08d" % [n.to_s(2)]).split('')
    { '111' => bs[0], '110' => bs[1],
      '101' => bs[2], '100' => bs[3], 
      '011' => bs[4], '010' => bs[5],
      '001' => bs[6], '000' => bs[7] }
  end

private

  def exec
    step, rows = 0, [@start]
    until (step += 1) == @steps
      rows << mutate(rows.last)
    end
    return rows
  end

  def mutate row
    row = %w(0 0) + row + %w(0 0)
    (0..row.length-3).map do |i|
      rule[row[i,3].join]
    end
  end
end
