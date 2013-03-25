#!/usr/bin/env ruby -w
require 'test/unit'
require './roman'

class TestRoman < Test::Unit::TestCase
  def test_decode
    assert_equal 2,   Roman::decode('II')
    assert_equal 4,   Roman::decode('IV')
    assert_equal 8,   Roman::decode('VIII')
    assert_equal 15,  Roman::decode('XV')
    assert_equal 31,  Roman::decode('XXXI')
    assert_equal 900, Roman::decode('CM')
  end

  def test_encode
    assert_equal 'II',   Roman::encode(2)
    assert_equal 'IV',   Roman::encode(4)
    assert_equal 'VIII', Roman::encode(8)
    assert_equal 'XV',   Roman::encode(15)
    assert_equal 'XXXI', Roman::encode(31)
    assert_equal 'CM',   Roman::encode(900)
  end
end
