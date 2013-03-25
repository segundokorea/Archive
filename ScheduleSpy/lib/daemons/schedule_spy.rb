#!/usr/bin/env ruby
require 'net/smtp'
require 'nokogiri'
require 'open-uri'

$stderr.print "Loading environment..."

# You might want to change this
ENV["RAILS_ENV"] ||= "production"

require File.dirname( __FILE__ ) + "/../../config/application"
Rails.application.require_environment!
Rails.logger.auto_flushing = true


$running = true
Signal.trap( 'TERM' ) do
  $running = false
end


while $running do
  # Send out the spies!
  working_spies = Spy.where :mission_complete => false
  if working_spies.length > 0
    interval = UPDATE_INTERVAL / working_spies.length

    working_spies.each do | spy |
      spy.update_status
      sleep interval
    end
  else
    $stderr.puts "No work; sleeping for #{NOWORK_INTERVAL} seconds..."
    sleep NOWORK_INTERVAL
  end
end