#!/usr/bin/ruby
# Updated 2010-01-10 by Sean Clemmer
#

# Overkill
class Case
  attr_reader :number, :value
  def initialize( number, value )
    @number = number
    @value  = value
  end
end


# Bring out the models! (initialize cases)
values = [
  0.01, 1, 5, 10, 25, 50, 75, 100, 200, 300, 400, 500, 750, 100, 5000, 10000,
  25000, 50000, 75000, 100000, 200000, 300000, 400000, 500000, 750000, 1000000  
].shuffle
i = 1
$cases = []
$chosen_cases = []
values.each do |value|
  $cases << Case.new( i, value )
  i += 1
end


def cases_left
  result = []
  $cases.each do |c|
    result << c.number
  end
  return result
end


def choose_from_cases_left
  choice = 0
  begin
    print "Choose a case [#{cases_left.join(", ")}]: "
    choice = gets.chomp.to_i
  end while !( cases_left.include? choice )
  $cases.each do |c|
    if c.number == choice
      $cases.delete( c )
      return c
    end
  end
end


$player_case = choose_from_cases_left
puts "You've chosen case number #{$player_case.number}. Let's play."


def make_offer( round_number )
  # This is where the fun is at
  sum = 0
  $cases.map do |x|
    sum += x.value
  end
  if $cases.length > 0
    average = sum / $cases.length
    offer = average * round_number * 0.1
    return offer
  else
    return $player_case * 0.5
  end
end

def deal_or_no_deal?
  deal = gets.chomp
  if deal =~ /^deal$/i
    return false
  elsif deal =~ /^no deal$/i
    return true
  else
    deal_or_no_deal?
  end
end


# Let's play!
no_deal = true
choices_per_round = 6 # Initially
round = 1
offer = 0
begin
  # Handle how many cases to open per round
  puts
  puts "This round you will choose #{choices_per_round} cases."

  # Make choices & open cases
  choices = []
  choices_per_round.times do
    choices << choose_from_cases_left
  end
  puts "You've opened the following cases:"
  choices.each do |choice|
    $chosen_cases << choice
    puts "  Case ##{choice.number} => $#{choice.value}"
  end

  # Banker makes an offer
  offer = make_offer( round )
  puts "The bank has offered you $#{offer}."

  # Deal or no deal?
  puts "Deal or no deal?"
  no_deal = deal_or_no_deal?

  if choices_per_round > 1
    choices_per_round -= 1
  end
  round += 1
end while no_deal and !$cases.empty?

if no_deal
  # Player is taking home their case
  puts "You've won... $#{$player_case.value}!"
else
  puts "You took the banker's offer of $#{offer}"
end
