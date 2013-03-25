#!/usr/bin/env ruby -w
require 'test/unit'
require './factorization'

class FactorTest < Test::Unit::TestCase
  def test_factorizations
    assert_equal [2],       Factorization.new(2).to_a
    assert_equal [3],       Factorization.new(3).to_a
    assert_equal [2,2],     Factorization.new(4).to_a
    assert_equal [5],       Factorization.new(5).to_a
    assert_equal [2,3],     Factorization.new(6).to_a
    assert_equal [2,5],     Factorization.new(10).to_a
    assert_equal [71],      Factorization.new(71).to_a
    assert_equal [2,2,5,5], Factorization.new(100).to_a
    assert_equal [1423],    Factorization.new(1423).to_a
  end
end
