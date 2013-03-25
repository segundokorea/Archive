require 'ostruct'
require './lexer'


class Chip8
  attr_reader :P # The program
  attr_reader :L # Length of program
  attr_reader :I # Current instruction
  attr_reader :V # Registers

  def initialize prog
    @P = prog.unpack('n*').map(&:byte)
    @L, @I, @V = @P.length, 0, Array.new(16, 0)
    exec
  end

  def registers ; @V.map(&:bin) end

  def to_s
    (0..@L-1).each do |i|
      inst = Lexer::tokenize(@P[i])
      puts "%03X - %s :: %s" % [i, @P[i], inst[:help]]
    end
  end

protected

  def nop d ; end

  def eof d ; puts "Bye!" end

  def op1NNN d ; @I = d.addr end

  def op3XKK d ; @I += 1 if val(d.vx) == d.kk end

  def op6XKK d ; store d.vx, d.kk end

  def op7XKK d
    sum, c = c8add val(d.vx), d.kk
    carry_add c
    store d.vx, sum
  end

  def op8XY0 d ; store d.vx, val(d.vy) end

  def op8XY1 d
    oring = c8or val(d.vx), val(d.vy)
    store d.vx, oring
  end

  def op8XY2 d
    anding = c8and val(d.vx), val(d.vy)
    store d.vx, anding
  end

  def op8XY3 d
    xoring = c8xor val(d.vx), val(d.vy)
    store d.vx, xoring
  end

  def op8XY4 d
    sum, c = c8add val(d.vx), val(d.vy)
    carry_add c
    store d.vx, sum
  end

  def op8XY5 d
    dif, c = c8sub val(d.vx), val(d.vy)
    carry_sub c
    store d.vx, dif
  end

  def op8X06 d
    rshift, b = c8rshift val(d.vx)
    carry_rshift b
    store d.vx, rshift
  end

  def op8XY7 d
    dif, c = c8sub val(d.vy), val(d.vx)
    carry_sub c
    store d.vx, dif
  end

  def op8X0E d
    lshift, b = c8lshift val(d.vx)
    carry_lshift b
    store d.vx, lshift
  end

  def opCXKK d
    rval = c8and rand(0..512), d.kk
    store d.vx, rval
  end

private

  def val n ; @V[n] end

  def store n, v ; @V[n] = v end

  def c8and a, b ; a & b end

  def c8or  a, b ; a | b end

  def c8xor a, b ; a ^ b end

  def c8add a, b ; sum = a + b ; [sum & 0xFF, sum >> 8] end

  def c8sub a, b ; dif = a - b ; [dif & 0xFF, -(dif >> 8)] end

  def c8rshift a ; [a >> 1, a & 1] end

  def c8lshift a ; [(a << 1) & 0xFF, a >> 7] end

  def carry_add i ; @V[0xF] = 0 ; @V[0xF] = 1 if i == 1 end

  def carry_sub i ; @V[0xF] = 1 ; @V[0xF] = 0 if i == 1 end

  def carry_rshift i ; @V[0xF] = 0 ; @V[0xF] = 1 if i == 1 end

  def carry_lshift i ; @V[0xF] = 0 ; @V[0xF] = 1 if i == 1 end

  def exec
    raise "No EOF found" unless @I < @L
    inst = Lexer::tokenize(@P[@I])
    raise "Invalid instruction #{inst}" unless Chip8.protected_instance_methods.include?(inst[:op])
    return nil if inst[:op] == :eof
    result = self.send inst[:op], inst[:data]
    puts "=> 0x%02X" % [result]
    @I += 1 ; exec
  end
end
