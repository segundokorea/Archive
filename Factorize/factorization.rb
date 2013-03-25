class Factorization
  @@memo = {}

  def initialize n
    raise ArgumentError if n < 2
    if memo.keys.include? n
      @factors ||= memo[n]
      return self
    end

    residue = n
    factors = []
    Math.sqrt(n).floor.downto(2).map do |m|
      if residue % m == 0
        residue /= m
        factors += Factorization.new(m).factors
        factors += Factorization.new(residue).factors
        break
      end
    end

    factors   = [n] if factors.empty?
    @@memo[n] = factors
    @factors  = factors
    return self
  end

  def prime? ; @factors.empty? end
  def factors ; @factors end
  def memo ; @@memo end
  def to_a ; @factors.sort end
  def to_s ; to_a.join(' * ') end
end
