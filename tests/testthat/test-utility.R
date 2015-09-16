msg.short <- "mooooo"
msg.long <- "I do not like green eggs and ham. I do not like them, Sam I Am!"

# --------
context("Utility functions")

describe("gsubv", {
    it("returns a character vector of the same length as x", {
      x <- c('one', 'two', 'three', 'four')
      expect_equal(length(gsubv('foo', 'bar', x[1])), 1)
      expect_equal(length(gsubv('foo', 'bar', x)), length(x))
    })

    it("can handle one pattern and replacement", {
      expect_equal(gsubv('foo', 'bar', 'batbangfoobazding'),
                   'batbangbarbazding')
      expect_equal(gsubv('f(.)o', 'b\\1r', 'batbangfoobazding'),
                   'batbangborbazding')
    })

    it("can handle n patterns and n replacements", {
      expect_equal(gsubv(c('one', 'two'), c('un', 'deux'), 'one two three!'), 'un deux three!')
    })

    it("can handle n patterns and m replacements, recycling the shorter", {
      # NOTE: remember replacements are done sequentially so actually recycling a pattern doesn't
      #  do anything unless we twice-replace something.
      expect_equal(gsubv(c('one', '[aeiou]'), c('un', 'a', 'e'), 'one two three!'), 'an twa thraa!')
      # you can totally recycle a replacement though
      expect_equal(gsubv(c('one', 'two', 'three'), c('un', 'deux'), 'one two three!'), 'un deux un!')
    })

    it("does multiple patterns/replacements sequentially", {
      ps <- c('cone', 'one')
      rs <- c('icecream waffle', 'a single')
      expect_equal(gsubv(ps, rs, 'one cone of cold'), 'a single icecream waffle of cold')
      expect_equal(gsubv(rev(ps), rev(rs), 'one cone of cold'), 'a single ca single of cold')

      ps <- c('cat', 'dog')
      rs <- c('ruler of dogs', 'puppy-wuppy')
      expect_equal(gsubv(ps, rs, 'A cat and a dog'), 'A ruler of puppy-wuppys and a puppy-wuppy')
      expect_equal(gsubv(rev(ps), rev(rs), 'A cat and a dog'), 'A ruler of dogs and a puppy-wuppy')
    })

    it("handles regex arguments", {
      expect_equal(gsubv(c('o', '([bc])[ae]'), c('a', '\\1o'), 'raining bears and cats and dogs'),
                   'raining boars and cots and dags')
    })

    it("passes through additional arguments to gsub", {
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
})

describe("wrap.message", {
    it("produces output the same as `strwrap`", {
      expect_equal(wrap.message(msg.long, 10), strwrap(msg.long, 10))
      expect_equal(wrap.message(msg.long, nchar(msg.long) + 1), strwrap(msg.long, nchar(msg.long) + 1))
    })
    it("splits on newlines before wrapping", {
      expect_equal(wrap.message(paste(paste(letters, collapse=''), msg.long, sep='\n'), 10),
                   strwrap(c(paste(letters, collapse=''), msg.long), 10))
    })
    it("allows input to be a character vector", {
      expect_equal(wrap.message(c(msg.long, msg.short), 10),
                   strwrap(c(msg.long, msg.short), 10))
    })
    it("does not remove empty lines", {
      expect_equal(wrap.message(c(msg.long, '', msg.short), 10),
                   strwrap(c(msg.long, '', msg.short), 10))
    })
    it("splits on embedded newlines inside a character vector", {
      expect_equal(wrap.message(c(msg.short, paste(msg.long, msg.short, collapse='\n')), 10),
                   strwrap(c(msg.short, msg.long, msg.short), 10))

    })
})

describe("get.message", {
    it("returns the argument if it's already character", {
        expect_equal(get.message('foobarbaz'), 'foobarbaz')
    })
    it("uses the custom print method if there is any", {
        m <- lm(Sepal.Length ~ Species, iris)
        # note print(m) is not the same as cat(m) for a lm
        expect_equal(get.message(m), capture.output(print(m)))
    })
    it("uses as.character otherwise", {
        expect_equal(get.message(1:3), as.character(1:3))
    })
    it("stores the name of the print method in attribute 'print.method' if store.print.method=TRUE", {
        m <- lm(Sepal.Length ~ Species, iris)
        expect_equal(attr(get.message(m, store.print.method=TRUE), 'print.method'), 'print.lm')
    })
    it("passes `...` on to the print method", {
        m <- t.test(runif(10))
        # t test prints with prefix 'FOOBAR\t' and digits 1, different to the default
        expected <- capture.output(print(m, prefix="FOOBAR\t", digits=1))
        expect_equal(get.message(m, prefix="FOOBAR\t", digits=1), expected)
    })
})

describe("has.print", {
    it("returns a boolean (by default)", {
        expect_is(has.print('foo'), 'logical')
        expect_is(has.print(1), 'logical')
    })
    it("returns whether the given object has a custom 'print' method (S3)", {
        # S3: methods(generic.function='print'). Apparently finds S4 too?
        # pm <- attr(methods(generic.function='print'), 'info') # pms$isS4
        # these have no special print methods
        expect_false(has.print('foobar'))
        expect_false(has.print(1))
        expect_false(has.print(1L))
        expect_false(has.print(TRUE))
        expect_false(has.print(1:5)) # vector
        expect_false(has.print(matrix(1:5)))
        expect_false(has.print(array(1:5)))
        expect_false(has.print(list(1:5)))

        # these do
        expect_true(has.print(data.frame(1:5)))
        expect_true(has.print(table(1:2, 1:2)))
        expect_true(has.print(t.test(runif(10))))
        # lm has a print.lm; the second is a mlm. There is no print.mlm but
        #   it inherits lm so should have print.lm
        expect_true(has.print(lm(Sepal.Length ~ Species, iris)))
        expect_true(has.print(lm(cbind(Petal.Length, Petal.Width) ~ Species, iris)))
    })
    it("returns the name of the print method if return.method is TRUE", {
        expect_false(has.print('foobar', return.method=TRUE))
        expect_false(has.print(1, return.method=TRUE))

        expect_equal(has.print(data.frame(1:5), return.method=TRUE), 'print.data.frame')
        expect_equal(has.print(table(1:2, 1:2), return.method=TRUE), 'print.table')
        expect_equal(has.print(t.test(runif(10)), return.method=TRUE), 'print.htest')
        # lm has a print.lm; the second is a mlm. There is no print.mlm but
        #   it inherits lm so should have print.lm
        expect_equal(has.print(lm(Sepal.Length ~ Species, iris), return.method=TRUE), 'print.lm')
        expect_equal(has.print(lm(cbind(Petal.Length, Petal.Width) ~ Species, iris), return.method=TRUE), 'print.lm')
    })
    it("works for S4 classes (whatever that means...)", {
        # they have a 'show' method rather than 'print'
        require(methods)
        setGeneric('print') # convert to standardGeneric so we use show,genericFunction-method
        expect_equal(class(print), 'standardGeneric', check.attributes=FALSE)
        expect_true(has.print(print))
        expect_equal(has.print(print, return.method=TRUE), 'show,genericFunction-method')
    })
})
