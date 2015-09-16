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
#' @family utility functions
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
#'   width `width`.
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
    return(strwrap(x, width=width))
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

