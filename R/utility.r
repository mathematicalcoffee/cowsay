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
#' @export
#' @family utility string functions
gsubv <- function (patterns, replacements, x, ...) {
    n <- max(length(patterns), length(replacements))
    patterns <- rep(patterns, length.out=n)
    replacements <- rep(replacements, length.out=n)
    for (i in seq_len(n)) {
        x <- gsub(patterns[i], replacements[i], x, ...)
    }
    return(x)
}

#' Attempts to convert `x` to character better than `as.character`.
#' @param x {character vector} thing to convert to character.
#' @param store.print.method {boolean} whether to store the name of the 'print'
#'         method used, if any (default `FALSE`).
#' @param ... passed on to the `print` method (e.g. a `width` argument)
#' @return {character vector}
#'  If `x` is a character, returns `x`.
#'  If `x` has a 'print' method ([`has.print`][has.print]), we capture the output
#'   of `print(x)`. This may have multiple elements (one per line of output).
#'  Otherwise, we use `as.character`.
#' If `store.print.method=TRUE`, the return value has an attribute
#' 'print.method' with the name of the method (see `::has.print`).
#' @details
#' The idea is to get a nicely-printed cowsay message regardless of input class.
#' @examples
#' m <- lm(Sepal.Length ~ Species, iris)
#' # compare
#' cat(as.character(m), sep='\n') # with
#' cat(cowsay:::get.message(m), sep='\n')
#' @family utility functions
#' @importFrom utils capture.output
get.message <- function (x, store.print.method=FALSE, ...) {
    # if `x` is character, leave as-is.
    # otherwise, if it has a 'print' method, capture that output.
    # otherwise, use `as.character`.
    if (is.character(x))
        return(x)
    if ((m <- has.print(x, return.method=TRUE)) != FALSE) {
        o <- utils::capture.output(print(x, ...))
        if (store.print.method)
            attr(o, 'print.method') <- m
        return(o)
    }
    return(as.character(x))
}

#' Wraps a message to a particular width.
#'
#' @inheritParams base::strwrap
#' @return {character vector} a vector of `x` chunked up into lines of approximately
#'   width `width`. We don't strip trailing spaces (if we were to wrap at a space).
#'   If there were embedded newlines in `x`, the input is also split up according
#'   to this.
#' @family utility functions
#' @seealso \code{\link[base]{strwrap}}
#' @examples
#' cowsay:::wrap.message('I do not like green eggs and ham.\nI do not like them, Sam I Am!',
#'              width=10)
wrap.message <- function (x, width=0.8 * getOption("width")) {
    # in case there are embedded newlines...
    # the ifelse is purely to not drop blank lines of '' in the `unlist`.
    x <- unlist(strsplit(ifelse(x == '', '\n', x), '\n'))
    return(strwrap.preserve.space(x, width=width))
}

#' Does this object have a 'print' method? (or 'show', for S4)?
#' @param x {anything} an object that we will query for a print method.
#' @param return.method {boolean} whether to return the method name, if one is found.
#' @return {boolean|character} If `x` has no `print` method, returns `FALSE`.
#'   If `x` *does* have print/show method(s), returns either `TRUE` (if `return.method=FALSE`,
#'   the default) or the name of the first method (`return.method=TRUE`).
#' @details
#'  This checks for the existence of an S3 `print` method, or an S4 `show`
#'  method on any of the classes of `x`. We ignore 'show,oldClass-method'.
#' @seealso [`methods`](utils::methods)
#' @family utility functions
#' @examples
#' # this returns TRUE as print.lm exists
#' cowsay:::has.print(lm(Sepal.Length ~ Species, iris))
#' # this returns TRUE because although print.mlm doesn't exist, print.lm *does*.
#' cowsay:::has.print(lm(cbind(Petal.Length, Petal.Width) ~ Species, iris))
#'
#' # Works for S4! (I think)
#' if (require(methods, quietly=T)) {
#'     setGeneric('print')
#'     class(print) # should be standardGeneric
#'     methods(class='standardGeneric') # 'show' is one of them
#'     cowsay:::has.print(print)
#'     cowsay:::has.print(print, return.method=TRUE) # show,genericFunction-method
#' }
has.print <- function (x, return.method=FALSE) {
    for (cls in class(x)) {
        m <- attr(utils::methods(class=cls), 'info')
        o <- rownames(m)[(m$generic == 'print' & !m$isS4) | (m$generic == 'show' & m$isS4)]
        o <- setdiff(o, 'show,oldClass-method') # not really sure what this is
        if (length(o))
            if (return.method)
                return(o[1])
            else
                return(TRUE)
    }
    return(FALSE)
}

#' Replace tabs with the appropriate number of spaces (one tab is 8 spaces)
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
#' replace.tabs('1234567\t9') # "1234567 9"
#' # the indent of 2 would push the 1234567 into the next 'column', so the 9
#' #  is pushed out to the next column again (character 17)
#' replace.tabs('1234567\t9', indent=2) # "1234567         9"
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
#' # @@TODO
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
