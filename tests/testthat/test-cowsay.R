quiet.cowsay = function (...) {
  out <- invisible(capture.output(cowsay(...)))
  return(out)
}
msg.short <- "mooooo"
msg.long <- "I do not like green eggs and ham. I do not like them, Sam I Am!"
default.rcowpath <- system.file('cows', 'default.rcow', package='cowsay')
default.rcow <- paste(readLines(default.rcowpath), collapse="\n")

context("cowsay")

# ---------------

test_that("cowsay outputs a message", {
    expect_message(cowsay('moo'))
})

test_that("cowsay returns a string equal to the outputted message", {
    out <- evaluate_promise(cowsay('moo'))
    expect_equal(out$result, x$messages)
})

test_that("cowsay's message is in the output", {
  # short msg
  expect_match(cowsay(msg.short), msg.short, fixed=T)
  
  # long msg
  expect_match(cowsay(msg.long, wrap=-1), msg.long, fixed=T)
})
test_that("cowsay's message is wrapped in a speech bubble", {
  expect_match(cowsay(msg.long, think=F, wrap=-1), construct.balloon(msg.long, think=F), fixed=T)  
})
# these are not the most comprehensive of tests...
test_that("cowsay's `cow` argument is respected", {
#@TODO  
})

test_that("cowsay's `eyes`, `tongue` and `think` arguments work", {
  expect_match(cowsay(msg.short, cow=default.rcowpath, eyes='EY', tongue='TT', think=T),
               gsubv(c('$thoughts', '$eyes', '$tongue'), c('o', 'EY', 'TT'), default.rcow, fixed=T),
               fixed=T)
  # eyes: first two characters used (padded to 2).
  # tongue: first two used (padded to 2)
  expect_match(cowsay(msg.short, cow=default.rcowpath, eyes='@.*', tongue='TONGUE', think=F),
               gsubv(c('$thoughts', '$eyes', '$tongue'), c('\\', '@.', 'TO'), default.rcow, fixed=T),
               fixed=T)
  expect_match(cowsay(msg.short, cow=default.rcowpath, eyes='@', tongue='T', think=F),
               gsubv(c('$thoughts', '$eyes', '$tongue'), c('\\', '@ ', 'T '), default.rcow, fixed=T),
               fixed=T)  
})
test_that("cowsay's `wrap` argument is respected", {
  # wrap -1: don't wrap
  expect_match(cowsay(msg.long, wrap=-1), msg.long, fixed=T)  
  # test wrapping
  wr <- round(nchar(msg.long)/2)
  expect_match(cowsay(msg.long, wrap=wr, think=F), construct.balloon(trim.message(msg.long, width=wr), think=F), fixed=T)  
})
test_that("cowsay's `style` argument is respected", {
  default.eyes <- 'oo'
  default.tongue <- '  '
  for (stylename in names(cow.styles)) {
    st <- cow.styles[[stylename]]
    eyes <- ifelse(is.null(st$eyes), default.eyes, st$eyes)
    tongue <- ifelse(is.null(st$tongue), default.tongue, st$tongue)
    
    expect_match(cowsay(msg.short, style=stylename),
                 cowsay(msg.short, eyes=eyes, tongue=tongue),
                 fixed=T)
  }
})
test_that("cowsay's `style` argument overrides supplied `eyes` and `tongue`", {
  # 'stoned' has eyes ** and tongue U
  expect_match(cowsay(msg.short, cow=default.rcowpath, style='stoned', eyes='Oo', tongue='  ', think=T),
               gsubv(c('$thoughts', '$eyes', '$tongue'), c('o', '**', 'U '), default.rcow, fixed=T),
               fixed=T)
})
test_that("cowsay gracefully handles a non-existent cow", {
  expect_error(cowsay('moo', cow='FDSAJKL'), regexp="Could not find the 'FDSAJKL' cowfile!", fixed=T)
})

context("randomcowsay")
test_that("randomcowsay randomizes the cow, style and think", {
  # this is not really a proper test, but run randomcowsay() 10 times with the same message and
  #  you shouldn't get the same output every single time
  # There are 47 cows, 9 cowstyles and 2 think values giving 846 possibilities so the probability
  #  of getting the same "random" cow 10 times in a row is very small (~5e-30)
  cows <- vapply(1:10, function (i) randomcowsay(msg.short), 'template')
  expect_more_than(length(unique(cows)), 1)
  
  # Try test that `cow` is the only thing randomized if `style` and `think` are fixed.
  # not a comprehensive test, but we check that:
  # * `cow` is actually randomized (more than one unique output produced)
  # * cow is the only thing randomized (check that it's in one of the possible cows with that style/think)
  cows <- vapply(1:10, function (i) randomcowsay(msg.short, style='paranoid', think=T), 'template')
  possible.cows <- vapply(list.cows(), randomcowsay, message=msg.short, style='paranoid', think=T, 'template')
  expect_more_than(length(unique(cows)), 1)
  expect_that(all(cows %in% possible.cows), is_true())
  
  # same for holding `style` fixed and varying the others
  cows <- vapply(1:10, function (i) randomcowsay(msg.short, cow='default', think=T), 'template')
  possible.cows <- vapply(names(cow.styles), randomcowsay, message=msg.short, cow='default', think=T, 'template')
  expect_more_than(length(unique(cows)), 1)
  expect_that(all(cows %in% possible.cows), is_true())
  
  # same for holding `think` fixed and varying the others (more tests required since 50% chance per test to randomly get it right)
  cows <- vapply(1:20, function (i) randomcowsay(msg.short, cow='default', style='dead'), 'template')
  possible.cows <- vapply(c(T, F), randomcowsay, message=msg.short, cow='default', style='dead', 'template')
  expect_more_than(length(unique(cows)), 1)
  expect_that(all(cows %in% possible.cows), is_true())
})

test_that("other arguments passed to `randomcowsay` override (note: `style` overrides `eyes` and `tongue` if present in the style)", {
  # all styles have eyes set, and all bar `stoned` do not set the tongue.
  # so eyes is *always* overidden, and tongue is only overidden if the style picked is `stoned`
  # we use the default cow because not all cows have tongues
  expect_match(randomcowsay(msg.short, cow='default', tongue='V '), 'V ', fixed=T)
  
  # override 'wrap'
  expect_match(randomcowsay(msg.long, wrap=10, think=F), construct.balloon(trim.message(msg.long, width=10), think=F),
               fixed=T)
  # other args are already tested in randomcowsay (think, cow, style)
})

context("list.cows")
test_that("list.cows() provides at least the default cow.", {
  expect_that('default.rcow' %in% list.cows(), is_true())  
})
test_that("list.cows(<path>) lists cows in that path", {
  path <- system.file('cows', package='cowsay')
  if (file.exists(path)) {
    cows = list.files(path, pattern='*.r?cow')
    expect_equal(sort(cows), sort(list.cows(path)))
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
  expect_equals(get.cowpaths(),
                c(paths, system.file('cows', package='cowsay')))
  Sys.setenv(COWPATH=oldenv)  
})

context("trim.message")
test_that("trim.message produces output the same as `strwrap`", {
  expect_equal(trim.message(msg.long, 10), strwrap(msg.long, 10))
  expect_equal(trim.message(msg.long, nchar(msg.long) + 1), strwrap(msg.long, nchar(msg.long) + 1))
})
test_that("trim.message splits on newlines before wrapping", {
  expect_equal(trim.message(paste(paste(letters, collapse=''), msg.long, sep='\n'), 10),
               strwrap(c(paste(letters, collapse=''), msg.long), 10))
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

# -------- UPTO

#' Reads a cow from a cowfile.
#' @template cowr
#' @details
#'
#' 1. If the file has extension `rcow`, we read it in as an R cow (\code{\link{read.cow.r}}).
#' 2. Otherwise, if the file assigns to `$the_cow` at some point, we assume it
#'      is a Perl cow and read it in with \code{\link{read.cow.perl}} or
#'      \code{\link{read.cow.noperl}} depending if you have Perl installed or not.
#' 3. Otherwise we just read it in as a plain cow (\code{\link{read.cow.plain}}).
#'
get.cow <- function (cowfile, eyes, thoughts, tongue) {
    cow <- NULL
    # 1. If it's an Rcow, read it in as such.
    if (grepl('\\.rcow$', cowfile)) {
        # 1. try R
        cow <- read.cow.r(cowfile, eyes=eyes, thoughts=thoughts, tongue=tongue)
        attr(cow, 'cowtype') <- 'R'

    # 2. If it's a Perl cow, read it in as such
    } else if (is.perl.cow(cowfile)) {
        PERL <- Sys.which('perl')
        if (PERL != '') {
            cow <- read.cow.perl(cowfile, eyes=eyes, thoughts=thoughts, tongue=tongue)
            attr(cow, 'cowtype') <- 'perl'
        } else {
            cow <- read.cow.noperl(cowfile, eyes=eyes, thoughts=thoughts, tongue=tongue)
            attr(cow, 'cowtype') <- 'noperl'
        }

    # 3. Otherwise, it's a plain cow.
    } else {
        cow <- read.cow.plain(cowfile, eyes=eyes, thoughts=thoughts, tongue=tongue)
        attr(cow, 'cowtype') <- 'plain'
    }

    return(cow)
}

#' Is a cow a Perl-cow?
#' A cow is a perl cow if `$the_cow` can be found in the file (rudimentary check).
#' @inheritParams get.cow
#' @family cowfile parsing
#' @return {boolean} whether the cow is a Perl cow or not.
is.perl.cow <- function (cowfile) {
    isperlcow <- file.exists(cowfile)
    if (!isperlcow) return(isperlcow)
    isperlcow <- isperlcow && length(grep('$the_cow', readLines(cowfile), fixed=T))
    return(isperlcow)
}

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
  
  expect_equals(read.cow.plain(f), eyes='XO', thoughts='[]', tongue='U ',
                gsubv(c('$eyes', '$thoughts', '$tongue'),
                      c('XO', '[]', 'U'),
                      cow,
                      fixed=T))
  unlink(f)
})
test_that("read.cow.plain ignores comments being lines starting with #", {
  f <- tempfile()
  cow <- 'MYCOW HAS EYES: $eyes\n and thoughts: $thoughts and tongue: $tongue\n'
  cmt <- '#here\'s a comment.'
  cat(paste(cmt, cow, sep='\n'), file=f)
  
  expect_equals(read.cow.plain(f), eyes='XO', thoughts='[]', tongue='U ',
                gsubv(c('$eyes', '$thoughts', '$tongue'),
                      c('XO', '[]', 'U'),
                      cow,
                      fixed=T))
  unlink(f)  
})

# UPTO NOTDONE

#' Read a cowfile (original Perl format)
#'
#' These are cow files from the original cowsay distribution, written in Perl.
#' We run them through a perl interpreter (if you have one installed); if you
#' don't have Perl installed we throw an error.
#' @template cowr
#' @example
#' \dontrun{
#' # if you have the original cowsay installed on your system...
#' cowfile <- '/usr/share/cowsay/cows/three-eyes.cow'
#' cat(read.cow.perl(cowfile, eyes="Oo", thoughts="\\", tongue="U"))
#' #         \  ^___^
#' #          \ (Ooo)\_______
#' #            (___)\       )\/\
#' #             U  ||----w |
#' #                 ||     ||
#' }
# To get around escaping problems:
# http://stackoverflow.com/questions/16632223/executing-perl-from-r-perlquote-shquote
read.cow.perl <- function (cowfile, eyes, thoughts, tongue, perl=Sys.which('perl')) {
    res <- NULL
    if (perl == '') {
        stop("You must have Perl installed and on your $PATH in order to read a Perl cowfile. Consider trying `read.cow.noperl`.")
    } else {
        # apparently system2 is better than system.
        res <- system2(perl,
                c('-e',
                  shQuote(sprintf('our ($thoughts, $eyes, $tongue) = @ARGV; do %s; print $the_cow', shQuote(cowfile))),
                  shQuote(thoughts),
                  shQuote(eyes),
                  shQuote(tongue)),
                stderr=F,
                stdout=T)        
        if (length(res) == 0) {
            warning("The resulting cow was empty; error in the cowfile?")
        } else {
            # add newline at end
            if (!grepl('^\\s*$', res[length(res)])) res=c(res, '')
            res <- paste(res, collapse="\n")
        }
  }
  return(res)
}

#' Reads a Perl-style cowfile where Perl is not installed (crude substitutions)
#'
#' @template cowr
#' @details
#' If you are trying to read in a Perl-style cowfile but Perl is not installed,
#' this tries to read it in and use some EXTREMELY rudimentary regex to parse
#' the cow:
#'
#' 1. Strip the `$the_cow = <<"EOC";` and `EOC` lines if they are there
#' 2. Replace `$eyes`, `$tongue` and `$thoughts` with the appropriate values
#'
#' Any lines before `$the_cow << EOC` and after the ending `EOC` are ignored.
#'
#' Everything else (including any other code) is read verbatim! So you really
#' don't want to be in the situation where you're using this function.
read.cow.noperl <- function (cowfile, eyes, thoughts, tongue) {
    lines <- readLines(cowfile)
    thecow <- grep('\\$the_cow *= *<< *', lines)
    if (length(thecow)) {
        cowline <- lines[thecow[1]]
        # trim out everything up to & including the '$the_cow << EOC;' line.
        lines <- lines[(thecow[1] + 1):length(lines)]

        # trim out everything after & including the final 'EOC' line.
        m <- regexpr('\\$the_cow *= *<< *"?([A-Za-z_]+)"? *; *$', cowline, perl=T)
        st <- attr(m, 'capture.start')
        if (m != -1) {
            EOC <- substr(cowline, st, st + attr(m, 'capture.length') - 1)
            endline <- grep(paste0('^', EOC, ' *$'), lines)
            if (length(endline)) {
                lines <- lines[1:(endline-1)]
            }
        }
    }
    # add newline at end
    if (!grepl('^\\s*$', lines[length(lines)])) lines=c(lines, '')
    cow <- paste(lines, collapse="\n")
    cow <- gsubv(c('$eyes', '$thoughts', '$tongue'),
                 c(eyes, thoughts, tongue),
                 cow, fixed=T)    
    return(cow)
}

# TODO: add newline to end of cows.
