class Array
  # Break an array into groups with
  # length in a given range
  def groups_of range
    range = range.to_a
    this = self.dup
    len = range.length
    res = []
    begin
      m = range[rand(len)]
      left = this.take(m)
      this.replace this.drop(m)
      res << left
    end until this.empty?
    return res
  end

  # Break an array into a specific number of
  # groups with length in a given range
  def groups_within n, range
    range = range.to_a
    loop do
      res = groups_of range
      return res if res.length == n
    end
  end
end



class Ticket
  attr_reader :rows, :id

  def initialize id, rows ; @id, @rows = id, rows end

  def to_s
    res = "Ticket ##{id}:"
    rows.each do |row|
      res += "\n+----+----+----+----+----+----+----+----+----+\n"
      res += '|' + row.map { |n| stringify n }.join('|') + '|'
    end
    res + "\n+----+----+----+----+----+----+----+----+----+"
  end

protected

  def stringify val
    return '    ' if val.nil?
    return ' %2d ' % val
  end
end



class Book
  attr_reader :book, :tickets

  # Use book parameter to prepopulate the book
  # Otherwise, a new book will be generated with randomized picks
  def initialize book=nil
    return @book = book unless book.nil?

    # A book has 6*3=18 rows and 9 columns for 162 positions
    @book = Array.new(162, nil)

    # First, divide the 90 numbers into 9 groups of 9 or 10
    ticket_picks = (1..90).to_a.groups_within(9, 9..10)

    # Now divide the picks into six tickets and randomize
    ticket_picks.map! do |p|
      p.shuffle.groups_within(6, 1..3).shuffle
    end

    # Transfer generated picks onto the book
    res = self
    ticket_picks.each_with_index do |tickets, c|
      c += 1 # Columns are 1-indexed
      tickets.each_with_index do |picks, t|
        t += 1 # Tickets are 1-indexed
        res = res.set_picks t, c, picks
      end
    end
    @book = res.book

    # Split book into tickets for ease of use
    @tickets = (1..6).map do |t|
      rows = (1..3).map { |r| @book[tridx(t, r), 9] }
      Ticket.new(t, rows)
    end
  end

  # Print out the book ticket-by-ticket
  def to_s ; tickets.map(&:to_s).join("\n\n") end

  # Get the value for a given ticket, row, and column
  def get t, r, c ; book[idx(t,r,c)] end

  # Set the value for a given ticket, row, and column
  def set t, r, c, val
    b = book.dup
    b[idx(t,r,c)] = val
    return Book.new(b)
  end

protected

  # Set a column on a given ticket
  # Uses picks in numeric order (nils randomly inserted)
  def set_picks t, c, picks
    picks   = picks.dup.sort
    choices = [1, 2, 3].shuffle.take(3 - picks.length)
    res     = self
    (1..3).each do |r|
      unless choices.include?(r)
        res = res.set t, r, c, picks.shift
      end
    end
    return res
  end

  def tidx t
    raise ArgumentError, "Invalid ticket #{t}" if t < 1 or t > 6
    return 27 * (t - 1)
  end

  def ridx r
    raise ArgumentError, "Invalid row #{r}" if r < 1 or r > 3
    return 9 * (r - 1)
  end

  def cidx c
    raise ArgumentError, "Invalid column #{c}" if c < 1 or c > 9
    return c - 1
  end

  def tridx t, r ; tidx(t) + ridx(r) end

  def idx t, r, c ; tridx(t, r) + cidx(c) end
end
