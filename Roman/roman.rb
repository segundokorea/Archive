module Roman
  Memo = [
    ['I',    1],
    ['IV',   4],
    ['V',    5],
    ['IX',   9],
    ['X',   10],
    ['XL',  40],
    ['L',   50],
    ['XC',  90],
    ['C',  100],
    ['CD', 400],
    ['D',  500],
    ['CM', 900],
    ['M', 1000]
  ]

  def self.encode n, result=''
    raise ArgumentError, "Numeral is out of bounds" if n < 1 or n > 3999
    lit, m  = Memo.dup.keep_if { |data| data[1] <= n }.last
    diff    = n - m
    result += lit
    return result if diff == 0
    return encode diff, result
  end


  def self.decode lit
    result = 0
    Memo.reverse_each do |data|
      num, m = data
      mdata = /\A#{num}(?<rest>.*)\z/.match(lit)
      if mdata
        result += m + decode(mdata[:rest])
        break
      end
    end
    return result
  end
end
