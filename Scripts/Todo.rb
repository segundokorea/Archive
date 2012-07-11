#!/usr/bin/ruby
require 'yaml'

# Need a file for our list
unless File.exists?( ".todo" )
  todo_file = File.open( '.todo', 'w' )
  todo_file.puts( '---' )
  todo_file.close
end

# Interpret our file
$todos = YAML::load_file( '.todo' )
unless $todos
  $todos = Array.new
end

def view
  if $todos.empty?
    puts "List empty."
  else
    $todos.each_index do |i|
      puts "#{i+1}. #{$todos[i]}"
    end
  end
end

def delete( items )
  items_to_delete = Array.new
  items.each do |item|
    unless $todos.nil?
      unless $todos[item].nil?
        items_to_delete << $todos[item]
      end
    end
  end
  items_to_delete.each do |item|
    $todos.delete( item )
  end
  puts "#{items_to_delete.length} items deleted."
  if $todos.empty?
    puts "List empty."
  end
end

def add( items )
  items.each do |item|
    $todos << item.to_s
  end
  puts "#{items.length} items added."
end

# Interpret arguments
case ARGV[0]
  when nil
    view
  when "-d"
    argv = Array.new
    ARGV.each do |arg|
      if arg =~ /^[0-9]*$/
        argv << arg.to_i - 1
      end
    end
    delete( argv )
  else
    add( ARGV )
end

# Write changes
File.open( '.todo', 'w' ) do |out|
  YAML::dump( $todos, out )
end

