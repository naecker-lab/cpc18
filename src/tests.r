# tests.r
# Tests for custom functions for CPC18 project
# Tests can be run by entering testthat::test_dir("src") in the console

test_that("calculating binomial probabilities works as expected", {
  expect_equal(calculate_prob(0, 0), 1)
  expect_equal(calculate_prob(1, 1), 0.5)
  expect_equal(calculate_prob(3, 0), 0.125)
  expect_equal(calculate_prob(3, 1), 0.375)
})

test_that("output of expand_lottery function has right dimenions", {
  expect_equal(length(expand_lottery(100, 0.5, 0, "Symm", 1)), 3)
  expect_equal(length(expand_lottery(100, 0.5, 0, "R-skew", 1)), 3)
  expect_equal(length(expand_lottery(100, 0.5, 0, "L-skew", 1)), 3)
})

test_that("expanded lotteries are right size", {
  expect_equal(length(expand_lottery(100, 0.5, 0, "Symm", 1)$outcomes), 2)
  expect_equal(length(expand_lottery(100, 0.5, 0, "Symm", 1)$probs), 2)
  expect_equal(length(expand_lottery(100, 0.5, 0, "R-skew", 1)$outcomes), 2)
  expect_equal(length(expand_lottery(100, 0.5, 0, "R-skew", 1)$probs), 2)
  expect_equal(length(expand_lottery(100, 0.5, 0, "L-skew", 1)$outcomes), 2)
  expect_equal(length(expand_lottery(100, 0.5, 0, "L-skew", 1)$probs), 2)
})

test_that("expanding lotteries does not change expected value", {
  expect_equal(expand_lottery(100, 0.5, 0, "Symm", 1)$EV, 50)
  expect_equal(expand_lottery(100, 0.5, 0, "R-skew", 1)$EV, 50)
  expect_equal(expand_lottery(100, 0.5, 0, "L-skew", 1)$EV, 50)
})
