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
gsubv <- function (patterns, replacements, x, ...) {
    n <- max(length(patterns), length(replacements))
    patterns <- rep(patterns, length.out=n)
    replacements <- rep(replacements, length.out=n)
    for (i in seq_len(n)) {
        x <- gsub(patterns[i], replacements[i], x, ...)
    }
    return(x)
}
