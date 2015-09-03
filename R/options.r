#' Querying/setting cowsay options.
#'
#' The following `cowsay` options are avilable:
#' * `rude.cows`: character vector (default: `c('sodomized', 'sodomized-sheep', 'head-in')`)
#'
#' Note: this code is shamelessly adapted from the `panderOptions` function in the
#' [pander](http://rapporter.github.io/pander/) package; it's just good and how
#' I would have done it anyway. Thanks muchly, and kudos to the pander team.
#' @references The corresponding function (`panderOptions`) of the pander package: https://github.com/Rapporter/pander/blob/master/R/options.R
#' @param o option name (string). See below.
#' @param value value to assign (optional)
#' @examples \dontrun{
#' cowsayOptions()
#' cowsayOptions('rude.cows')
#' cowsayOptions('rude.cows', c(cowsayOptions('rude.cows'), 'kiss'))
#' }
#' @export
cowsayOptions <- function(o, value) {

    res <- getOption('cowsay')

    ## just querying
    if (missing(value)) {

        if (missing(o))
            return(res)

        if (o %in% names(res))
            return(res[[o]])

        msg <- paste('Wrong option queried.',
                     'Possible `cowsay` options:',
                     paste('-', names(res), collapse='\n'),
                     sep='\n')
        stop(msg)
    } else {

        if (!o %in% names(res))
            stop(paste('Invalid option name:', o))

        ## fix assigning NULL to a list element
        if (is.null(value)) {
            res[o] <- list(NULL)
        } else {
            res[[o]] <- value
        }

        options('cowsay' = res)

    }

}
