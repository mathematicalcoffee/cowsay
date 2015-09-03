# options
context('cowsayOptions')

default.options <- list(
                rude.cows=c('sodomized', 'sodomized-sheep', 'head-in')
            )

test_that("cowsayOptions() returns a list with all of and only cowsay's options", {
    o <- cowsayOptions()
    expect_that(o, is_a('list'))
    expect_that(o, equals(default.options))
})

test_that("cowsayOptions(option) returns the value of that option", {
    for (n in names(default.options)) {
        expect_that(cowsayOptions(n), equals(default.options[[n]]),
                    info=sprintf("cowsayOptions('%s') doesn't retrieve the expected options", n))
    }
    # can't test more than one option yet, because I only have one.
})

test_that("cowsayOptions(nonexistent.option) gives an error", {
    expect_that(cowsayOptions('NONEXISTENT'), throws_error("Wrong option queried."))
    expect_that(cowsayOptions('NONEXISTENT'), throws_error("Possible `cowsay` options:"))
    expect_that(cowsayOptions('NONEXISTENT'), throws_error(paste('-', names(default.options), collapse='\n')))
})

test_that("cowsayOptions(option, value) assigns a value to an option", {
    cowsayOptions('rude.cows', c('foo', 'bar'))
    expect_that(cowsayOptions('rude.cows'), equals(c('foo', 'bar')))
    cowsayOptions('rude.cows', default.options$rude.cows)
})
