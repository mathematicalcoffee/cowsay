msg.short <- "mooooo"
msg.long <- "I do not like green eggs and ham. I do not like them, Sam I Am!"

# --------

context("gsubv")

test_that("gsubv returns a character vector of the same length as x", {
  x <- c('one', 'two', 'three', 'four')
  expect_equal(length(gsubv('foo', 'bar', x[1])), 1)
  expect_equal(length(gsubv('foo', 'bar', x)), length(x))
})

test_that("gsubv can handle one pattern and replacement", {
  expect_equal(gsubv('foo', 'bar', 'batbangfoobazding'),
               'batbangbarbazding')
  expect_equal(gsubv('f(.)o', 'b\\1r', 'batbangfoobazding'),
               'batbangborbazding')
})

test_that("gsubv can handle n patterns and n replacements", {
  expect_equal(gsubv(c('one', 'two'), c('un', 'deux'), 'one two three!'), 'un deux three!')
})

test_that("gsubv can handle n patterns and m replacements, recycling the shorter", {
  # NOTE: remember replacements are done sequentially so actually recycling a pattern doesn't
  #  do anything unless we twice-replace something.
  expect_equal(gsubv(c('one', '[aeiou]'), c('un', 'a', 'e'), 'one two three!'), 'an twa thraa!')
  # you can totally recycle a replacement though
  expect_equal(gsubv(c('one', 'two', 'three'), c('un', 'deux'), 'one two three!'), 'un deux un!')
})

test_that("when gsubv has multiple patterns/replacements, they are done sequentially", {
  ps <- c('cone', 'one')
  rs <- c('icecream waffle', 'a single')
  expect_equal(gsubv(ps, rs, 'one cone of cold'), 'a single icecream waffle of cold')
  expect_equal(gsubv(rev(ps), rev(rs), 'one cone of cold'), 'a single ca single of cold')
  
  ps <- c('cat', 'dog')
  rs <- c('ruler of dogs', 'puppy-wuppy')
  expect_equal(gsubv(ps, rs, 'A cat and a dog'), 'A ruler of puppy-wuppys and a puppy-wuppy')
  expect_equal(gsubv(rev(ps), rev(rs), 'A cat and a dog'), 'A ruler of dogs and a puppy-wuppy')
})

test_that("gsubv handles regex arguments", {
  expect_equal(gsubv(c('o', '([bc])[ae]'), c('a', '\\1o'), 'raining bears and cats and dogs'),
               'raining boars and cots and dags')
})

test_that("gsubv passes through additional arguments to gsub", {
  # ignore.case
  expect_equal(gsubv('hello', 'hi', 'HELLO there', ignore.case=F), 'HELLO there')
  expect_equal(gsubv('hello', 'hi', 'HELLO there', ignore.case=T), 'hi there')
  
  # fixed
  expect_equal(gsubv('.', 'FULLSTOP', 'Hi...', fixed=T), 'HiFULLSTOPFULLSTOPFULLSTOP')
  
  # perl
  expect_equal(gsubv('\\.(?!\\.)', 'FULLSTOP', 'Hi...', perl=T), 'Hi..FULLSTOP')
  
  # useBytes
  # ... dunno.
})

context("wrap.message")
test_that("wrap.message produces output the same as `strwrap`", {
  expect_equal(wrap.message(msg.long, 10), strwrap(msg.long, 10))
  expect_equal(wrap.message(msg.long, nchar(msg.long) + 1), strwrap(msg.long, nchar(msg.long) + 1))
})
test_that("wrap.message splits on newlines before wrapping", {
  expect_equal(wrap.message(paste(paste(letters, collapse=''), msg.long, sep='\n'), 10),
               strwrap(c(paste(letters, collapse=''), msg.long), 10))
})
test_that("wrap.message allows input to be a character vector", {
  expect_equal(wrap.message(c(msg.long, msg.short), 10),
               strwrap(c(msg.long, msg.short), 10))
})
test_that("wrap.message does not remove empty lines", {
  expect_equal(wrap.message(c(msg.long, '', msg.short), 10),
               strwrap(c(msg.long, '', msg.short), 10))
})
test_that("wrap.message splits on embedded newlines inside a character vector", {
  expect_equal(wrap.message(c(msg.short, paste(msg.long, msg.short, collapse='\n')), 10),
               strwrap(c(msg.short, msg.long, msg.short), 10))

})
