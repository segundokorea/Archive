#!/usr/bin/env ruby
# = Name
# University of Chicago TimeSchedules Spy (ScheduleSpy)
#
# = Description
# The ScheduleSpy watches the University of Chicago TimeSchedules system for
# openings in the classes you want. Pass the system the name of the classes,
# and watch for Growl notifications keeping you abreast of any openings.
#
# = License
# Copyright (c) 2010, Sean Clemmer
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. All advertising materials mentioning features or use of this software
#    must display the following acknowledgement:
#    This product includes software developed by Sean Clemmer.
# 4. Neither the name Sean Clemmer nor the names of its contributors may be
#    used to endorse or promote products derived from this software without
#    specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY SEAN CLEMMER ''AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL SEAN CLEMMER BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# Author:: Sean Clemmer
# Version:: 01
# License:: BSD Four-clause
require 'ruby-growl'
require 'nokogiri'
require 'open-uri'


# Trying to be very lax with arguments; most people don't care about looking
# up schedules from three years ago, so we focus on the course first. A search
# mode would help even more...
usage = "USAGE: #{__FILE__} DEPT[ -]COURSE [SECTION [SEASON[ YEAR]]]"


# Secret option: -h or --help
if ARGV[0] =~ /^-h|--help$/i
  $stderr.puts usage
  Kernel::exit
end


class Time
  # === Usage
  # Compute the current season from the current month
  #
  # === Note
  # Based on when users would be looking up TimeSchedules (Summer will be
  # ignored, sorry!)
  def season
    month = self.month
    if 9 <= month and month <= 11   # September to November
      return :autumn
    elsif 3 <= month and month <= 5 # March to May
      return :spring
    elsif 6 <= month and month <= 8 # April to August
      return :summer
    else                            # December to February
      return :winter
    end
  end
end


# Pseudo-globals to be populated by the argument parsing process
department = nil              # Four-letter department identifier
course     = nil              # Five-digit course number
section    = 1                # One-to-two-digit section number
season     = Time.now.season  # Symbol for season
year       = Time.now.year    # Four-digit year
offset     = 0                # For evaluating arguments


# Should probably move to a real argument-handling system instead of a bunch
# of case statements; alternatively, don't accept such lax arguments...
case ARGV[0]
when /^[a-z]{4}[ -]{0,1}\d{5}$/i
  # Matched a department with a course
  argument   = $&
  department = /^[a-z]{4}/i.match(argument).to_s.upcase
  course     = /\d{5}$/.match(argument).to_s.to_i
when /^[a-z]{4}$/i
  # Matched just a department
  department = $&.upcase

  case ARGV[1]
  when /^\d{5}$/
    # The course should come next...
    course = $&.to_i
  else
    $stderr.puts usage
    raise RuntimeError, 'Invalid COURSE (67)'
  end

  offset += 1 # Because the course number was its own argument
else
  $stderr.puts usage
  raise RuntimeError, 'Invalid DEPT[ -]COURSE (73)'
end

# We may have been given a section number
if ARGV[1+offset]
  case ARGV[1+offset]
  when /^\d{1,2}$/
    # Matched a one-or-two-digit section number
    section = $&.to_i
    offset += 1 # Section was it's own argument
  end
end

# An optional season and year may follow
if ARGV[1+offset]
  case ARGV[1+offset]
  when /^[a-z]+\d{2,4}$/i
    # Matched a season with a year
    argument = $&
    season   = /^[a-z]+/i.match(argument).to_s
    year     = /\d+$/.match(argument).to_s

    case season
    when /^(a|autumn|f|fall)$/i
      season = :autumn
    when /^(w|winter)$/i
      season = :winter
    when /^(s|spring)$/i
      season = :spring
    else
      $stderr.puts usage
      raise RuntimeError, 'Invalid SEASON (104)'
    end

    case year
    when /\d{4}$/
      year = $&.to_i
    when /\d{2}$/
      year = 2000 + $&.to_i
    else
      $stderr.puts usage
      raise RuntimeError, 'Invalid YEAR (114)'
    end

  when /^[a-z]+$/i
    # Matched only a season
    season = $&

    case season
    when /^(a|autumn|f|fall)$/i
      season = :autumn
    when /^(w|winter)$/i
      season = :winter
    when /^(s|spring)$/i
      season = :spring
    else
      $stderr.puts usage
      raise RuntimeError, 'Invalid SEASON (129)'
    end

    # Handle a year, if given
    # Otherwise, assume the year is the current one
    case ARGV[1+offset+1]
    when /^\d{4}$/
      year = $&.to_i
    when /^\d{2}$/
      year = 2000 + $&.to_i
    end
  else
    $stderr.puts usage
    raise RuntimeError, 'Invalid SEASON[ YEAR] (142)'
  end
end


# Need to calculate the term number that TimeSchedules expects
# First, to calculate the term number, we need seasonal offsets
# Then it's relatively simple to calculate the term from 1983 onward
seasons = {
  :autumn =>  1,
  :winter => -1,
  :spring => 0
}

term = seasons[season] + 3 * ( year - 1983 ) # Translate everything into the term number


# Some more pseudo-globals we'll need for interacting with TimeSchedules
found_an_opening = false
course_pattern   = "%s%s%02i" % [department, course, section]
requested_url    = "http://timeschedules.uchicago.edu/view.php?dept=#{department}&term=#{term}"
season           = season.to_s.capitalize # Convert from symbol to string


# Using Growl to send pretty updates on the search status
# In the future, may add a pretty icon and what not
growl = Growl.new "localhost", "ruby-growl", ["ruby-growl Notification"]
growl.notify "ruby-growl Notification", "Schedule Watcher", "Starting script.\nLooking for %s-%s (%02i) [%s %i]..." % [department, course, section, season, year]


while not found_an_opening
  enrolled = 0 # Number of students currently enrolled in the course
  maximum  = 0 # Maximum number of students able to enroll

  # Grab the Time Schedules page and crawl through those ugly tables
  response = Nokogiri::HTML(open(requested_url))
  response.xpath('//span[@class="smallredt"]/ancestor::a/ancestor::td/ancestor::tr').each do |class_id|
    if class_id.xpath('./td/a/span[@class="smallredt"]').text.gsub(/\W/,'') == course_pattern
      enrolled = class_id.xpath('./td/span')[7].text.gsub(/\W/,'').to_i
      maximum  = class_id.xpath('./td/span')[8].text.gsub(/\W/,'').to_i
    end
  end

  # See if there are any openings
  # If so, by George, we've got it! Clean up and exit
  # Otherwise, keep looking, but wait a few minutes before trying again
  if enrolled < maximum
    growl.notify "ruby-growl Notification", "Schedule Watcher", "Stopping script.\n%s-%s (%02i)  [%s %i] has an opening!" % [department, course, section, season, year]
    found_an_opening = true
  else
    growl.notify "ruby-growl Notification", "Schedule Watcher", "Continuing script.\n%s-%s (%02i)  [%s %i] has no openings..." % [department, course, section, season, year]
    sleep 300 # Five minutes
  end
end