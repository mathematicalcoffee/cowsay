#' 
#' @seealso \code{\link{gsub}}
gsubv <- function (patterns, replacements, x, ...) {
    n <- max(length(patterns), length(replacements))
    patterns <- rep(patterns, length.out=n)
    replacements <- rep(replacements, length.out=n)
    for (i in seq_len(n)) {
        x <- gsub(patterns[i], replacements[i], x, ...)
    }
}
