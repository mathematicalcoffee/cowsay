context("gsubv")

test_that("gsubv returns a character vector of the same length as x", {
  x = c('one', 'two', 'three', 'four')
  expect_equal(length(gsubv('foo', 'bar', x[1])), 1)
  expect_equal(length(gsubv('foo', 'bar', x)), length(x))
})
test_that("gsubv can handle one pattern and replacement", {
  expect_equal(gsubv('foo', 'bar', 'batbangfoobazding'),
               'batbangbarbazding')
  expect_equal(gsubv('f(.)o', 'b\\1r', 'batbangfoobazding'),
               'batbangborbazding')
})
# TODO: SEQUENTIAL APPLICATION. And add to documentation too.
test_that("gsubv can handle n patterns and n replacements", {

})
test_that("gsubv can handle n patterns and m replacements, recycling the shorter", {
  
})
test_that("gsubv passes through additional arguments")