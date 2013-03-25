require 'ostruct'

class Fixnum
  def bin
    return "%08b" % self
  end

  def xx
    return "%01X" % self if self < 10
    return "%02X" % self
  end

  def xxx
    return "%03X" % self
  end

  def byte
    return "%04X" % self
  end
end



module Lexer
  def self.tok recipe
    return ->(p, op) {
      p.match(op) do |m|
        data = OpenStruct.new
        data.vx   = m[:vx].to_i(16) rescue nil
        data.vy   = m[:vy].to_i(16) rescue nil
        data.kk   = m[:kk].to_i(16) rescue nil
        data.addr = m[:addr].to_i(16) rescue nil
        token = recipe.call data
        token[:data] = data
        return token
      end
    }
  end

  def self.tokenize inst
    TOKENS.inject(nil) do |memo, (op, match)|
      match[op, inst] || memo
    end
  end

  HEX  = /[0123456789ABCDEF]/
  X    = /(?<vx>#{HEX})/
  Y    = /(?<vy>#{HEX})/
  KK   = /(?<kk>#{HEX}{2})/
  NNN  = /(?<addr>#{HEX}{3})/

  TOKENS = {
    /0000/       => tok(lambda { |d| { :op => :eof, :help => "EOF" }}),
    /1#{NNN}/    => tok(lambda { |d| { :op => :op1NNN, :help => "Jump to #{d.addr.xxx}" }}),
    /3#{X}#{KK}/ => tok(lambda { |d| { :op => :op3XKK, :help => "Skip if V#{d.vx.xx} == #{d.kk.xx}" }}),
    /6#{X}#{KK}/ => tok(lambda { |d| { :op => :op6XKK, :help => "V#{d.vx.xx}  = #{d.kk.xx}" }}),
    /7#{X}#{KK}/ => tok(lambda { |d| { :op => :op7XKK, :help => "V#{d.vx.xx} += #{d.kk.xx}" }}),
    /8#{X}#{Y}0/ => tok(lambda { |d| { :op => :op8XY0, :help => "V#{d.vx.xx}  = V#{d.vy.xx}" }}),
    /8#{X}#{Y}1/ => tok(lambda { |d| { :op => :op8XY1, :help => "V#{d.vx.xx} |= V#{d.vy.xx}" }}),
    /8#{X}#{Y}2/ => tok(lambda { |d| { :op => :op8XY2, :help => "V#{d.vx.xx} &= V#{d.vy.xx}" }}),
    /8#{X}#{Y}3/ => tok(lambda { |d| { :op => :op8XY3, :help => "V#{d.vx.xx} ^= V#{d.vy.xx}" }}),
    /8#{X}#{Y}4/ => tok(lambda { |d| { :op => :op8XY4, :help => "V#{d.vx.xx} += V#{d.vy.xx}" }}),
    /8#{X}#{Y}5/ => tok(lambda { |d| { :op => :op8XY5, :help => "V#{d.vx.xx} -= V#{d.vy.xx}" }}),
    /8#{X}06/    => tok(lambda { |d| { :op => :op8X06, :help => "V#{d.vx.xx} >> #{1.xx}" }}),
    /8#{X}#{Y}7/ => tok(lambda { |d| { :op => :op8XY7, :help => "V#{d.vx.xx}  = V#{d.vy} - V#{d.vx.xx}" }}),
    /8#{X}0E/    => tok(lambda { |d| { :op => :op8X0E, :help => "V#{d.vx.xx} << #{1.xx}" }}),
    /C#{X}#{KK}/ => tok(lambda { |d| { :op => :opCXKK, :help => "V#{d.vx.xx}  = RAND & #{d.kk.xx}" }})
  }
end
