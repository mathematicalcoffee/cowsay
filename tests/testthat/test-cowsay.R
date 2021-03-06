quiet.cowsay = function (...) {
  out <- suppressMessages(cowsay(...))
  return(out)
}
msg.short <- "mooooo"
msg.long <- "I do not like green eggs and ham. I do not like them, Sam I Am!"
default.rcowpath <- system.file('cows', 'default.rcow', package='cowsay')
default.rcow <- paste(readLines(default.rcowpath), collapse="\n")

perl <- Sys.which('perl')[1]

context("cowsay")

# ---------------

test_that("cowsay outputs a message", {
    expect_message(cowsay('moo'))
})

test_that("cowsay returns a string equal to the outputted message", {
  # can't use evaluate_promise unless testthat is new enough: see
  # https://github.com/hadley/testthat/commit/b51f8473285a981ca2c6cfc72ad7558a9cab6a38
  # (as of 16 Jan 2015 this fix has not made it to CRAN)
  messages <- character()
  mHandler = function (m) {
    messages <<- c(messages, m$message)
    invokeRestart("muffleMessage")
  }
  o <- withCallingHandlers(cowsay('moo'), message=mHandler)
  expect_equivalent(messages, o)
})

test_that("cowsay's message is in the output", {
  # short msg
  expect_match(quiet.cowsay(msg.short), msg.short, fixed=T)

  # long msg
  expect_match(quiet.cowsay(msg.long, wrap=-1), msg.long, fixed=T)
})
test_that("cowsay's message is wrapped in a speech bubble", {
  expect_match(quiet.cowsay(msg.long, think=F, wrap=-1), construct.balloon(msg.long, think=F), fixed=T)
})
# these are not the most comprehensive of tests...
test_that("cowsay's `cow` argument is respected", {
#@TODO
})

test_that("cowsay's `eyes`, `tongue` and `think` arguments work", {
  expect_match(quiet.cowsay(msg.short, cow=default.rcowpath, eyes='EY', tongue='TT', think=T),
               gsubv(c('$thoughts', '$eyes', '$tongue'), c('o', 'EY', 'TT'), default.rcow, fixed=T),
               fixed=T)
  # eyes: first two characters used (padded to 2).
  # tongue: first two used (padded to 2)
  expect_match(quiet.cowsay(msg.short, cow=default.rcowpath, eyes='@.*', tongue='TONGUE', think=F),
               gsubv(c('$thoughts', '$eyes', '$tongue'), c('\\', '@.', 'TO'), default.rcow, fixed=T),
               fixed=T)
  expect_match(quiet.cowsay(msg.short, cow=default.rcowpath, eyes='@', tongue='T', think=F),
               gsubv(c('$thoughts', '$eyes', '$tongue'), c('\\', '@ ', 'T '), default.rcow, fixed=T),
               fixed=T)
})
test_that("cowsay's `wrap` argument is respected", {
  # wrap -1: don't wrap
  expect_match(quiet.cowsay(msg.long, wrap=-1), msg.long, fixed=T)
  # test wrapping
  wr <- round(nchar(msg.long)/2)
  expect_match(quiet.cowsay(msg.long, wrap=wr, think=F), construct.balloon(wrap.message(msg.long, width=wr), think=F), fixed=T)  
})
test_that("cowsay's `style` argument is respected", {
  default.eyes <- 'oo'
  default.tongue <- '  '
  for (stylename in names(cow.styles)) {
    st <- cow.styles[[stylename]]
    eyes <- ifelse(is.null(st$eyes), default.eyes, st$eyes)
    tongue <- ifelse(is.null(st$tongue), default.tongue, st$tongue)

    expect_match(quiet.cowsay(msg.short, style=stylename),
                 quiet.cowsay(msg.short, eyes=eyes, tongue=tongue),
                 fixed=T)
  }
})
test_that("cowsay's `style` argument overrides supplied `eyes` and `tongue`", {
  # 'stoned' has eyes ** and tongue U
  expect_match(quiet.cowsay(msg.short, cow=default.rcowpath, style='stoned', eyes='Oo', tongue='  ', think=T),
               gsubv(c('$thoughts', '$eyes', '$tongue'), c('o', '**', 'U '), default.rcow, fixed=T),
               fixed=T)
})
test_that("cowsay gracefully handles a non-existent cow", {
  expect_error(cowsay('moo', cow='FDSAJKL'), regexp="Could not find the 'FDSAJKL' cowfile!", fixed=T)
})

context("randomcowsay")
quiet.randomcowsay = function (...) {
  out <- suppressMessages(randomcowsay(...))
  return(out)
}
test_that("randomcowsay randomizes the cow, style and think", {
  all.cows <- list.cows(rude=TRUE)
  n <- length(all.cows) * 2

  # this is not really a proper test, but run randomcowsay() many times with the same message and
  #  you shouldn't get the same output every single time (we'll generate twice
  #  as many cows as there are to be sure...)
  # There are 47 cows, 9 cowstyles and 2 think values giving 846 possibilities so the probability
  #  of getting the same "random" cow 10 times in a row is very small (~5e-30)
  cows <- replicate(n, quiet.randomcowsay(msg.short))
  expect_more_than(length(unique(cows)), 1)

  # Try test that `cow` is the only thing randomized if `style` and `think` are fixed.
  # not a comprehensive test, but we check that:
  # * `cow` is actually randomized (more than one unique output produced)
  # * cow is the only thing randomized (check that it's in one of the possible cows with that style/think)
  cows <- replicate(n, quiet.randomcowsay(msg.short, style='paranoid', think=T))
  possible.cows <- vapply(all.cows, quiet.randomcowsay, message=msg.short, style='paranoid', think=T, 'template')
  expect_more_than(length(unique(cows)), 1)
  expect_that(all(cows %in% possible.cows), is_true())

  # same for holding `style` fixed and varying the others
  cows <- replicate(n, quiet.randomcowsay(msg.short, cow='default', think=T))
  possible.cows <- vapply(names(cow.styles), quiet.randomcowsay, message=msg.short, cow='default', think=T, 'template')
  expect_more_than(length(unique(cows)), 1)
  expect_that(all(cows %in% possible.cows), is_true())

  # same for holding `think` fixed and varying the others (more tests required since 50% chance per test to randomly get it right)
  cows <- replicate(n, quiet.randomcowsay(msg.short, cow='default', style='dead'))
  possible.cows <- vapply(c(T, F), quiet.randomcowsay, message=msg.short, cow='default', style='dead', 'template')
  expect_more_than(length(unique(cows)), 1)
  expect_that(all(cows %in% possible.cows), is_true())

  # can't really test if the `rude` argument is obeyed...
  # but if we sample twice as many cows as there are, surely we'll pick up a rude
  #  cow at least once (if it is broken)
  o <- cowsayOptions('rude')
  cowsayOptions('rude', TRUE)
  cows <- replicate(n, quiet.randomcowsay(msg.short, style='dead', think=FALSE, rude=FALSE))
  rude.cows <- vapply(cowsayOptions('rude.cows'), quiet.cowsay, 'template', message=msg.short, style='dead', think=FALSE)
  expect_that(any(cows %in% rude.cows), is_false())
  cowsayOptions('rude', o)
  # unfortunately we can't try to check that there *is* a rude cow generated because they're just not very
  # frequent (only 3 in 40-something) so we can't really guarantee to get one.
})

test_that("other arguments passed to `randomcowsay` override (note: `style` overrides `eyes` and `tongue` if present in the style)", {
  # all styles have eyes set, and all bar `stoned` do not set the tongue.
  # so eyes is *always* overidden, and tongue is only overidden if the style picked is `stoned`
  # we use the default cow because not all cows have tongues
  # blah, also style 'stoned' replaces the tongue!
  expect_match(quiet.randomcowsay(msg.short, cow='default', style='greedy', tongue='V '), 'V ', fixed=T)

  # override 'wrap'
  expect_match(quiet.randomcowsay(msg.long, wrap=10, think=F), construct.balloon(wrap.message(msg.long, width=10), think=F),
               fixed=T)
  # other args are already tested in randomcowsay (think, cow, style)
})

context("list.cows")
test_that("list.cows() provides at least the default cow.", {
  expect_that('default.rcow' %in% list.cows(), is_true())
})
test_that("list.cows(<path>) lists cows in that path, and respects its `rude` argument", {
  path <- system.file('cows', package='cowsay')
  if (file.exists(path)) {
    cows = list.files(path, pattern='*.r?cow')
    expect_equal(sort(cows), sort(list.cows(path, rude=TRUE)))

    rude.cows <- cowsayOptions('rude.cows')
    rude.cows <- c(paste0(rude.cows, '.cow'), paste0(rude.cows, '.rcow'))
    cows <- setdiff(cows, rude.cows)
    expect_equal(sort(cows), sort(list.cows(path, rude=FALSE)))
  }
})

context("get.cowfile")
test_that("get.cowfile returns the input argument if it's an existing file with extension 'cow' or 'rcow'", {
  cat('foobar', file=(f <- tempfile(fileext='.rcow')))
  expect_equal(get.cowfile(f), f)
  unlink(f)

  cat('foobar', file=(f <- tempfile(fileext='.cow')))
  expect_equal(get.cowfile(f), f)
  unlink(f)

  # if wrong extension or doesn't exist
  f <- tempfile(fileext='.cow')
  if (file.exists(f)) unlink(f)
  expect_null(get.cowfile(f))
  cat('foobar', file=(f <- tempfile()))
  expect_null(get.cowfile(f))
  unlink(f)
})

test_that("get.cowfile returns an .rcow over a .cow if both exist", {
  # relies on get.cowpath() where we set COWPATH env var working...
  f <- tempfile()

  oldenv <- Sys.getenv('COWPATH')
  Sys.setenv(COWPATH=dirname(f))
  cat('foobar', file=paste0(f, '.rcow'))
  cat('foobar', file=paste0(f, '.cow'))
  # try with fileXYZ as the cow
  expect_equal(get.cowfile(basename(f)), paste0(f, '.rcow'))

  # should also work with fileXYZ.rcow, fileXYZ.cow as the cow
  expect_equal(get.cowfile(paste0(basename(f), '.rcow')), paste0(f, '.rcow'), info="Having the cow extension in the filename should still work.")
  expect_equal(get.cowfile(paste0(basename(f), '.cow')), paste0(f, '.rcow'), info="Having the cow extension in the filename should still work.")

  unlink(paste0(f, '.rcow'))
  unlink(paste0(f, '.cow'))
  Sys.setenv(COWPATH=oldenv)
})

context("get.cowpaths")
test_that('get.cowpaths includes the environment variable $COWPATH (if set), splitting on colons', {
  oldenv <- Sys.getenv('COWPATH')
  paths <- c('foo/bar', '/baz/bat/bang')
  Sys.setenv(COWPATH=paste(paths, collapse=.Platform$path.sep))
  expect_that(all(paths %in% get.cowpaths()), is_true())

  Sys.setenv(COWPATH=oldenv)
})

test_that('get.cowpaths includes the package path "cows"', {
  expect_that(system.file('cows', package='cowsay') %in% get.cowpaths(), is_true())
})

test_that('get.cowpaths has the $COWPATH paths before the package path', {
  oldenv <- Sys.getenv('COWPATH')
  paths <- c('foo/bar', '/baz/bat/bang')
  Sys.setenv(COWPATH=paste(paths, collapse=.Platform$path.sep))
  expect_equal(get.cowpaths(),
                c(paths, system.file('cows', package='cowsay')))
  Sys.setenv(COWPATH=oldenv)
})

# note: does no wrapping
context("construct.balloon")
test_that("construct.balloon includes the original message", {
  expect_match(construct.balloon(msg.short, think=T), msg.short, fixed=T)
  expect_match(construct.balloon(msg.long, think=T), msg.long, fixed=T)
})

test_that("construct.balloon handles multi-line messages (one line per element)", {
  msg <- strsplit(construct.balloon(c(msg.short, msg.long), think=F), '\\n')[[1]]
  # msg[1] is the top bubble
  expect_match(msg[2], msg.short, fixed=T)
  expect_match(msg[3], msg.long, fixed=T)
})

test_that("construct.balloon thought bubbles have '_' as the top boundary, '-' as the bottom, '(' and ')' as the sides.", {
  msg <- strsplit(construct.balloon(c(msg.short, msg.long), think=T), '\\n')[[1]]
  n <- max(nchar(c(msg.short, msg.long)))
  expect_equal(length(msg), 4)
  expect_match(msg[1], paste0(' ', paste(rep('_', n + 2), collapse='')), fixed=T) # '+ 2' because space padding. First space for the side-of-bubble
  expect_match(msg[2], sprintf(sprintf('( %%-%ds )', n), msg.short), fixed=T)
  expect_match(msg[3], sprintf(sprintf('( %%-%ds )', n), msg.long), fixed=T)
  expect_match(msg[4], paste0(' ', paste(rep('-', n + 2), collapse='')), fixed=T) # '+ 2' because space padding. First space for the side-of-bubble
})
test_that("construct.balloon speech bubbles have '_' as the top boundary, '-' as the bottom, '<' and '>' as the sides if one line.", {
  msg <- strsplit(construct.balloon(msg.long, think=F), '\\n')[[1]]
  n <- nchar(msg.long)
  expect_equal(length(msg), 3)
  expect_match(msg[1], paste0(' ', paste(rep('_', n + 2), collapse='')), fixed=T) # '+ 2' because space padding. First space for the side-of-bubble
  expect_match(msg[2], sprintf('< %s >', msg.long), fixed=T)
  expect_match(msg[3], paste0(' ', paste(rep('-', n + 2), collapse='')), fixed=T) # '+ 2' because space padding. First space for the side-of-bubble
})
test_that("construct.balloon speech bubbles have '_' as the top boundary, '-' as the bottom, '|' and '|' as the sides if multiline.", {
  # three-line message: need corners and | sides
  msg <- strsplit(construct.balloon(c(msg.short, msg.long, msg.short), think=F), '\\n')[[1]]
  n <- max(nchar(c(msg.short, msg.long)))
  expect_equal(length(msg), 5)
  expect_match(msg[1], paste0(' ', paste(rep('_', n + 2), collapse='')), fixed=T) # '+ 2' because space padding. First space for the side-of-bubble
  expect_match(msg[2], sprintf(sprintf('/ %%-%ds \\', n), msg.short), fixed=T)
  expect_match(msg[3], sprintf(sprintf('| %%-%ds |', n), msg.long), fixed=T)
  expect_match(msg[4], sprintf(sprintf('\\ %%-%ds /', n), msg.short), fixed=T)
  expect_match(msg[5], paste0(' ', paste(rep('-', n + 2), collapse='')), fixed=T) # '+ 2' because space padding. First space for the side-of-bubble
})

context("get.cow")
fr <- tempfile(fileext=".rcow")
frr <- sub('\\.rcow$', '.r', fr)
fperl <- tempfile(fileext=".cow")
fplain <- tempfile(fileext=".cow")

cat(default.rcow, "", sep="\n", file=fr)
cat("eyes <- tolower(eyes)\n", file=frr)
cat("$the_cow = <<EOC;", "moocow $eyes", "EOC\n", sep='\n', file=fperl)
cat("$eyes $tongue $thoughts\n", file=fplain)

cow.r <- get.cow(fr, eyes='OO', tongue='u', thoughts='\\')
cow.perl <- get.cow(fperl, eyes='OO', tongue='u', thoughts='\\')
cow.plain <- get.cow(fplain, eyes='OO', tongue='u', thoughts='\\')

test_that("get.cow returns a single string with attribute 'cowtype' being the type of cow", {
  expect_is(cow.r, 'character')
  expect_equal(length(cow.r), 1)
  expect_equal(attr(cow.r, 'cowtype'), 'R')

  expect_is(cow.perl, 'character')
  expect_equal(length(cow.perl), 1)
  expect_equivalent(attr(cow.perl, 'cowtype'), ifelse(perl == "", "noperl", "perl"))

  expect_is(cow.plain, 'character')
  expect_equal(length(cow.plain), 1)
  expect_equal(attr(cow.plain, 'cowtype'), 'plain')
})
test_that("get.cow reads it as an R cow if it has extension .rcow", {
  # equivalent disregards attributes
  expect_equivalent(cow.r, read.cow.r(fr, eyes='OO', tongue='u', thoughts='\\'))
})
test_that("get.cow reads it as a Perl cow if it is a Perl cow, using Perl if you've got it and read.cow.noperl otherwise", {
  expect_equivalent(cow.perl, ifelse(perl == "", read.cow.noperl(fperl, eyes='OO', tongue='u', thoughts='\\'), read.cow.perl(fperl, eyes='OO', tongue='u', thoughts='\\')))
})
test_that("get.cow reads it as a plain cow otherwise", {
  expect_equivalent(cow.plain, read.cow.plain(fplain, eyes='OO', tongue='u', thoughts='\\'))
})
test_that("get.cow throws the appropriate errors if they are encountered", {
  # perl
  if (perl == "") {
    skip("cannot test whether Perl errors throw errors; no Perl installed")
  } else {
    cat('foobarbaz', '$the_cow=LKSJDF', sep='\n', file=fperl, append=F)
    expect_error(get.cow(fperl, eyes='OO', tongue='u', thoughts='\\'), "error in the cowfile")
  }

  # r
  cat('foobar\n', file=frr, append=T)
  expect_error(get.cow(fr, eyes='OO', tongue='u', thoughts='\\'), "object 'foobar' not found")
})

unlink(frr)
unlink(fr)
unlink(fperl)
unlink(fplain)

context("is.perl.cow")
test_that("is.perl.cow returns FALSE if the cowfile doesn't exist", {
  f <- tempfile()
  unlink(f)
  expect_false(is.perl.cow(f))
})
test_that("is.perl.cow returns FALSE if the cowfile doesn't have extension .cow", {
  f <- tempfile()
  unlink(f)
  expect_false(is.perl.cow(f))
})
test_that("is.perl.cow returns TRUE when the string '$the_cow' is in the file (and the cowfile exists and has extension .cow)", {
  f <- tempfile(fileext=".cow")
  expect_false(is.perl.cow(f)) # right extension, exists, no $the_cow
  cat('$the_cow\n', file=f)
  expect_true(is.perl.cow(f))
  unlink(f)
})

context("read.cow.r")
test_that("read.cow.r treats the .rcow file as a plain cow", {
  f <- tempfile(fileext='.rcow')
  cow <- 'MYCOW HAS EYES: $eyes\n and thoughts: $thoughts and tongue: $tongue\n'
  cat(cow, file=f)

  expect_equal(read.cow.r(f, eyes='XO', thoughts='[]', tongue='U '),
               read.cow.plain(f, eyes='XO', thoughts='[]', tongue='U '),
               fixed=T)
  unlink(f)
})
test_that("if an R file exists with the same name as the cow, this is executed BEFORE the .rcow is read", {
  f <- tempfile(fileext='.rcow')
  fr <- sub('\\.rcow$', '.r', f)

  rcode <- 'eyes <- tolower(eyes)\n'
  cow <- 'MYCOW HAS EYES: $eyes\n and thoughts: $thoughts and tongue: $tongue\n'
  cat(cow, file=f)
  cat(rcode, file=fr)

  expect_equal(read.cow.r(f, eyes='XO', thoughts='[]', tongue='U '),
               read.cow.plain(f, eyes='xo', thoughts='[]', tongue='U '),
               fixed=T)
  unlink(f)
  unlink(fr)
})

test_that("if the R file has an error, this is thrown", {
  f <- tempfile(fileext='.rcow')
  fr <- sub('\\.rcow$', '.r', f)

  rcode <- 'eyes <- TOLOWER(eyes)\n'
  cow <- 'MYCOW HAS EYES: $eyes\n and thoughts: $thoughts and tongue: $tongue\n'
  cat(cow, file=f)
  cat(rcode, file=fr)

  expect_error(read.cow.r(f, eyes='XO', thoughts='[]', tongue='U '),
               'could not find function "TOLOWER"')
  unlink(f)
  unlink(fr)
})

context("read.cow.plain")

test_that("read.cow.plain reads a cow as-is and simply substitutes the $eyes, $thoughts, $tongue strings", {
  f <- tempfile()
  cow <- 'MYCOW HAS EYES: $eyes\n and thoughts: $thoughts and tongue: $tongue\n'
  cat(cow, file=f)

  expect_equal(read.cow.plain(f, eyes='XO', thoughts='[]', tongue='U '),
               gsubv(c('$eyes', '$thoughts', '$tongue'),
                     c('XO', '[]', 'U '),
                     cow,
                     fixed=T))
  unlink(f)
})
test_that("read.cow.plain ignores comments being lines starting with #", {
  f <- tempfile()
  cow <- 'MYCOW HAS EYES: $eyes\n and thoughts: $thoughts and tongue: $tongue\n'
  cmt <- '#here\'s a comment.'
  cat(paste(cmt, cow, sep='\n'), file=f)

  expect_equal(read.cow.plain(f, eyes='XO', thoughts='[]', tongue='U '),
              gsubv(c('$eyes', '$thoughts', '$tongue'),
                    c('XO', '[]', 'U '),
                    cow,
                    fixed=T))
  unlink(f)
})


# note: these tests are not "proper", you need perl really
context("read.cow.perl")

test_that("if Perl cnanot be found, read.cow.perl throws an error", {
  expect_error(read.cow.perl(get.cowfile('default'), eyes='oo', thoughts='o', tongue='U', perl=''), 'You must have Perl installed')
})

# no perl
if (perl == '') {
  skip("cannot do remaining read.cow.perl tests, no Perl installed")
} else {
  f <- tempfile(fileext='.cow')
  # 'chop' returns the last character of $eyes
  cat('## A cow', '$extra = chop($eyes);', '$eyes .= ($extra x 2);', '$the_cow = "$eyes|$thoughts|$tongue"', file=f, sep='\n')
  o <- read.cow.perl(f, eyes='@#', thoughts='&', tongue='U ')

  test_that("read.cow.perl produces a single string as output", {
    expect_is(o, 'character')
    expect_equal(length(o), 1)
  })
  test_that("read.cow.perl runs the cowfile through perl, passing the eyes/thoughts/tongue correctly", {
    # adds a newline at the end
    expect_equal(o, "@##|&|U \n")
  })
  test_that("read.cow.perl escapes appropriately", {
    expect_equal(read.cow.perl(f, eyes='"\'', thoughts='$PATH', tongue='\"'),
                 '"\'\'|$PATH|"\n')
  })
  unlink(f)
}

# crude substitutions of perl code
context("read.cow.noperl")

f <- tempfile(fileext=".cow")
cat("this line should be ignored", "$the_cow=<<EOC;", "cow is: $eyes, $thoughts, $tongue", "second line $tongue", "EOC", "this line should also be ignored", sep='\n', file=f)
o <- read.cow.noperl(f, eyes='"\'', thoughts="$t", tongue="U")
o.l <- strsplit(o, '\n')[[1]]
test_that("read.cow.noperl returns a single string with a newline on the end", {
  expect_is(o, 'character')
  expect_equal(length(o), 1)
  expect_equal(substr(o, nchar(o), nchar(o)), '\n')
})
test_that("read.cow.noperl ignores lines before <<EOC and after EOC", {
  expect_false(grepl('ignored', o, fixed=T))
  expect_equal(length(o.l), 2) # 2-line cow
})
test_that("read.cow.noperl returns the lines between $the_cow = <<EOC and the last EOC", {
  expect_match(o.l[1], "cow is:", fixed=T)
  expect_match(o.l[2], "second line ", fixed=T)
})
test_that("read.cow.noperl substitutes $eyes, $thoughts and $tongue", {
  expect_equal(o.l[1], "cow is: \"', $t, U")
  expect_equal(o.l[2], "second line U")
})
test_that("read.cow.noperl returns all the lines if $the_cow = <<EOC is not there", {
  cat("here is my test cow", "here is the second line", sep="\n", file=f, append=F)
  expect_equal(read.cow.noperl(f, eyes="oo", thoughts="o", tongue="U"), "here is my test cow\nhere is the second line\n")
})
test_that("read.cow.noperl handles multiple HEREDOC syntax: surrounded by quotes, different token, spacing", {
  # spacing (though techinically there should be no space between << and EOC, and now you are supposed to quote the EOC too)
  cat("$the_cow      =  << EOC   ;  ", "mycow", "EOC  ", file=f, sep='\n', append=F)
  expect_equal(read.cow.noperl(f, eyes="oo", thoughts="\\", tongue="U"), "mycow\n", info="whitespace surrounding marker")

  # different token
  cat("$the_cow = <<MYMARKER;", "EOC", "MYMARKER", file=f, sep='\n', append=F)
  expect_equal(read.cow.noperl(f, eyes="oo", thoughts="\\", tongue="U"), "EOC\n", info="different marker name")

  # quotes around marker
  cat("$the_cow = <<\"FOOBAR\";", "thecow", "FOOBAR", file=f, sep='\n', append=F)
  expect_equal(read.cow.noperl(f, eyes="oo", thoughts="\\", tongue="U"), "thecow\n", info="quotes around marker")
})
unlink(f)
