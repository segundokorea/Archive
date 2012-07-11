#!/usr/bin/env ruby
require 'nokogiri'
require 'open-uri'

ARGV.each do |arg|
  request  = 'http://www.google.com/search?q=' + arg.gsub( /\W/, '_' )
  response = Nokogiri::HTML( open( request ) )
  position = 0
  response.xpath( "//li[@class='g']" ).each do |result|
    address = result.xpath( "//li[@class='g'][#{position}]/h3/a/@href" ).first
    unless address.nil?
      address = address.content
      title   = result.xpath( "//li[@class='g'][#{position}]/h3" ).first.content
      puts title + " => " + address
    end
    position += 1
  end
end