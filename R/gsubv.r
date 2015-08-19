#' Performs multiple sequential replacements
#' 
#' This modifies `x` replacing all occurences of `patterns` with `replacements`,
#' working sequentially through the patterns and replacments (matching them up with
#' each other).
#' 
#' @param patterns {character vector} the patterns to replace.
#'  Regex is permitted. If shorter than `replacements`, it is recycled
#'  to that length.
#' @param replacements {character vector} the replacements. Should be the
#'  same length as `patterns`. If not, it is recycled to that length.
#' @param x {character vector} string to perform replacements on.
#'  If it has more than one elements, the entire set of substitutions will be
#'  performed on each element.
#' @param ... passed through to `gsub` (e.g. `ignore.case`, `perl`)
#' @details
#' Note that replacements are done sequentially, so that it is possible that
#' changing the order of the patterns/replacements will change the output (see
#' examples).
#' @return character vector of the same length as `x` where all occurences of
#' `patterns` have been replaced by the corresponding element of `replacements`,
#' sequentially.
#' @examples
#' gsubv('foo', 'bar', 'foobar') # barbar
#' 
#' # Multiple patterns/replacements
#' gsubv(c('one', 'two'), c('un', 'deux'), 'one two three!')
#' # [1] "un deux three!"
#' 
#' # Multiple replacements are done sequentially; be careful about the order
#' gsubv(c('cone', 'one'), c('icecream waffle', 'a single'), 'one cone of cold')
#' # [1] "a single icecream waffle of cold"
#' gsubv(c('cone', 'one')[2:1], c('icecream waffle', 'a single')[2:1], 'one cone of cold')
#' # [1] "a single ca single of cold"
#' 
#' # Replacements can be made on previous replacements
#' gsubv(c('cat', 'dog'), c('ruler of dogs', 'puppy-wuppy'), 'A cat and a dog')
#' # [1] "A ruler of puppy-wuppys and a puppy-wuppy"
#' gsubv(c('cat', 'dog')[2:1], c('ruler of dogs', 'puppy-wuppy')[2:1], 'A cat and a dog')
#' # [1] "A ruler of dogs and a puppy-wuppy"
#' 
#' # You can use regex of course, as usual.
#' gsubv(c('o', '([bc])[ae]'), c('a', '\\1o'), 'raining bears and cats and dogs')
#' # [1] "raining boars and cots and dags"
#' 
#' @seealso \code{\link{gsub}}
#' @family utility string functions
#' @export
gsubv <- function (patterns, replacements, x, ...) {
    n <- max(length(patterns), length(replacements))
    patterns <- rep(patterns, length.out=n)
    replacements <- rep(replacements, length.out=n)
    for (i in seq_len(n)) {
        x <- gsub(patterns[i], replacements[i], x, ...)
    }
    return(x)
}

#' Replace tabs with the appropriate number of spaces
#' @param x {character vector} character vector to replace tabs. Each element is
#'         treated as a separate line; embedded newlines are not.
#' @param indent {integer} if the string will be indented by some number of characters,
#'         indicate so here. This will affect how the tabs will be calculated!
#'         The indent is not actually added on to the input string, though.
#' @return {character vector} a character vector of the same length as `x` with all
#'        '\t' replaced by the number of spaces they were to take up if you `cat`-ed
#'        `x`
#'
#' If you split up your terminal into columns of width 8, a '\t' puts the next
#'  bit of content at the start of the next column. So the '\t' consumes the
#'  number of spaces needed until the string before the '\t' is of a length divisible
#'  by 8. If the string is already a length divisible by 8, we add 8 spaces.
#'
#' It should be the case that `cat(x)` and `cat(replace.tabs(x))` appear the
#'  same width on the terminal (though the former will have less characters than
#'  the latter if it has a tab that is replaced by spaces in the latter).
#' @examples
#' @family utility string functions
replace.tabs <- function (x, indent=0) {
    has.tabs <- grep('\t', x)

    bits <- strsplit(x[has.tabs], '\t', fixed=TRUE, useBytes=TRUE)
    x[has.tabs] <- vapply(bits,
           function (chunks) {
               n <- length(chunks) # \t tabs out to next column of length 8
               ncs <- nchar(chunks[-n])
               ncs[1] <- ncs[1] + indent
               # format(x, width=6) counts escapes like '\t' as 2 characters, '\' and 't'
               # We use sprintf which doesn't have this problem.
               paste(c(strpad(chunks[-n], ceiling((ncs + 1)/8) * 8), chunks[n]),
                     collapse='')
           }, '')
    x
}

#' Pads a string out to a particular width (with spaces).
#' @param x {character vector} string(s) to pad
#' @param width {integer} width to pad out to, of same length as `x` or recycled out to this length.
#' @param left {boolean} whether to left-justify (the default) or right-justify.
#' @return {character vector} A vector of same length as `x` where `nchar(output) == width`.
#' @details
#' Just a wrapper round `sprintf`. Note we can't use `format(x, width=width)` because
#'  this counts escape characters like `\t` as two characters ('\' and 't'), rather
#'  than 1, and hence does not add enough padding.
#'
#' It should always be that `all(nchar(strpad(x, width)) == width)`.
#' @family utility string functions
#' @examples
#' x <- '123\t456\t789' # nchar(x) == 11
#' strpad(x, width=15)
#' nchar(strpad(x, width=15)) == 15
#' # compare to:
#' format(x, width=15)
#' nchar(format(x, width=15)) # 13!
#' @seealso [`format`](base::format), [`sprintf`](base::sprintf)
strpad <- function (x, width, left=TRUE) {
    sprintf(paste0(ifelse(left, '%-', '%'), width, 's'), x)
}

#' Wraps a string to a particular width, preserving spaces (except some trailing ones)
#' @param x {character vector} string(s) to wrap
#' @param width {integer} maximum width of each line
#' @return {character vector} a character vector with one element per line of `x`,
#'    after wrapping.
#' @details
#' Like R's strwrap but does not destroy whitespace as that one does.
#' In fact, it's a good deal dumber than strwrap.
#'
#' It's a bit quirky, too: it will drop trailing whitespace, but **only** if keeping
#'  it would have made the line longer than `width`.
#'
#' I'm not really convinced on it.
#'
#' The idea is that you can `cowsay` things where spaces should stay the same, like
#'  tables. Though, I suppose you should have `wrap=F` for these.
#'
#' Note: tabs are not substituted with the appropriate number of spaces, if you want
#'  that you need to call [replace.tabs][] first.
#' @examples
#' x <- '123    456'
#' @seealso [`strwrap`](base::strwrap)
#' @family utility string functions
strwrap.preserve.space <- function (x, width) {
    x <- strsplit(x, '\n')
    x[!lengths(x)] <- '' # but keep the empty lines...
    lines <- strsplit(unlist(x),
                      '(?<=\\s)(?=\\S)|(?<=\\S)(?=\\s)', # speed up?
                      perl=T)
    lines <- lapply(lines,
                    function (bits) {
                        if (length(bits) == 0)
                            return('')
                        if (length(bits) <= 2) # [text] or [text, space]
                            return(bits[1])
                        lens <- nchar(bits)
                        upto <- 0
                        lne <- ''
                        is.space <- F
                        for (i in seq_along(bits)) {
                            if (upto + lens[i] > width) {
                                upto <- 0 # reset
                                if (is.space) {
                                    lne <- paste0(lne, '\n')
                                } else {
                                    lne <- paste0(lne, '\n', bits[i])
                                }
                            } else {
                                lne <- paste0(lne, bits[i])
                                upto <- upto + lens[i]
                            }
                            is.space <- !is.space
                        }
                        strsplit(lne, '\n', fixed=T)[[1]]
                    })
    unlist(lines)
}
