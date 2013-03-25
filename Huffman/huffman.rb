class Array
  # Transform and sum all values
  def sum &transform
    return reduce(:+) unless transform
    return self.map { |x| transform[x] }.reduce(:+)
  end

  # Group values in arrays of size n
  def groups_of n
    a = self
    result = []
    begin
      left, a = a.take(n), a.drop(n)
      result << left
    end while not a.empty?
    return result
  end

  # Pad last child with X if group isn't long enough
  def fill_groups_of n, x
    l = self.pop
    r = n - l.length
    r.times { |i| l << x }
    self.push l
    return r
  end
end


# Calculate frequencies for each unit in the message
def freqs meth, msg
  raise ArgumentError, "Invalid iterator for String" unless [:chars, :codepoints, :each].include? meth
  freqs = {}
  msg.send(meth.to_sym).each do |c|
    freqs[c] = (freqs[c] || 0) + 1
  end
  return freqs
end


# Represent the Huffman Tree (or any other, really)
class Node
  attr_reader :key, :value, :left, :right
  def terminal? ; left.nil? and right.nil? end
  def initialize key, value, left=nil, right=nil
    @key, @value, @left, @right = key, value, left, right
  end
end



# Huffman Encoder / Decoder
# Use Marshal to serialize the encoding
class Huffman
  attr_reader :message, :data, :encoding, :padding

  def initialize message
    chars     = message.chars.to_a
    tree      = build_tree chars
    encoding  = build_encoding chars, tree

    # Encode the message and split into bytes (0-padded)
    bytes     = message.chars.map { |c| encoding[c] }.join.split('').groups_of(8)
    padding   = bytes.fill_groups_of 8, '0'
    data      = bytes.flatten.map(&:to_i)

    @data     = data
    @message  = message
    @encoding = encoding
    @padding  = padding
  end

  def marshal_dump
    [@padding, @encoding, @data]
  end

  def marshal_load obj
    @padding, @encoding, @data = obj
    @message = recover
  end

protected

  # Recover the encoded message
  def recover data=@data, encoding=@encoding, padding=@padding
    message = ''
    data = data.take(data.length - padding)
    begin
      encoding.each do |char, enc|
        bits = enc.split('').map(&:to_i)
        len  = bits.length
        if data.take(len) == bits
          message << char
          break
        end
      end
      data = data.drop len
    end until data.empty?
    return message
  end


  # Build the encoding from paths on the Huffman Tree
  def build_encoding chars, tree, encoding={}
    chars.uniq.each { |c| encoding[c] = path_to c, tree }
    return encoding
  end


  # Walk down the Huffman Tree, noting the path
  def path_to char, node, path=''
    return path if node.key == char
    return '' if node.terminal?
    return path_to(char, node.left, path + '0') + path_to(char, node.right, path + '1')
  end


  # Build the Huffman Tree
  def build_tree arr, fs=nil, units=nil, score=nil
    fs    ||= freqs :each, arr
    units ||= arr.uniq
    score ||= units.sum { |c| fs[c] }

    return nil if fs == {}

    c, f = fs.reduce do |u,v|
      _, uf = u ; _, vf = v
      uf > vf ? u : v
    end

    fs.delete c
    left  = Node.new c, f
    right = nil
    if units.length == 2
      c1, f1 = fs.shift
      right = Node.new c1, f1
    else
      right = build_tree( arr, fs, units - [c], score - f )
    end

    return Node.new units, score, left, right
  end
end
