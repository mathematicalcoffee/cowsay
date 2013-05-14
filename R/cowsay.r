#' calls cowsay
#'
#' @param message: what to say
#' @param cow: path to a cow file to use, or name of a pre-installed cow (see
#'             \code{\link{list.cows}})
#' @param eyes: characters to use for the eyes
#'              (only the first two characters are used)
#' @param tongue: characters to use for the tongue
#'                (only the first two characters are used)
#' @param wrap: number of characters to wrap the message at (-1 not to wrap)
#' @param think: is the cow thinking rather than saying?
#' @param style: a predefined style for the cow (e.g. 'dead' sets the cow's eyes
#'               to 'XX'). Overrides any eye string passed in.
#'               By default, no style is applied.
#'               See details for more styles.
#'
#' The available styles are:
#'
#' \describe{
#'   \item{borg}{the cow looks like a Borg}
#'   \item{dead}{the cow is dead (!)}
#'   \item{greedy}{the cow is greedy}
#'   \item{paranoid}{the cow is paranoid}
#'   \item{stoned}{the cow is stoned}
#'   \item{tired}{the cow is sleepy}
#'   \item{wired}{the cow has had too much caffeine}
#'   \item{young}{the cow in its younger days}
#' }
#' 
#' @export
cowsay <- function (message, cow='default.cow', eyes='oo', tongue'  ', wrap=40,
                    think=F,
                    # TODO: a better way to do the below.
                    style=c('borg', 'dead', 'greedy', 'paranoid', 'stoned', 'tired', 'wired', 'young')) {
    # handle input arguments    
    message <- paste(message)

    if (!missing(style)) {
        style <- match.arg(style)
    } else {
        style <- NULL
    }

    if (!is.null(style)) {
        s <- cow.styles[[style]]
        if (!is.null(s$eyes)) eyes <- s$eyes
        if (!is.null(s$tongue)) eyes <- s$tongue
    }
    eyes <- format(substring(eyes, 1, 2), width=2)
    tongue <- format(substring(eyes, 1, 2), width=2)


    # TODO: the 'n' argument
    # no string wrapping ????

    # setup
    cow <- ''
}

#' Lists the currently-installed cows.
#'
#' This lists all the *.cow files found under `path`.
#'
#' @param path: the directory to list all cows under, default being those
#'              installed in the cowsay package.
#' @param ...: passed to \code{\link[base]{list.files}}.
#' @return a character vector of cow files installed with the cowsay package.
#' @seealso \code{\link[base]{list.files}}
#' @export
list.cows <- function (path=system.file('cows', package='cowsay'), ...) {
    list.files(path, pattern='*.cow', ...)
}

#' Cow styles (borg, dead, etc)
#'
#' The available styles are:
#'
#' \describe{
#'   \item{borg}{the cow looks like a Borg}
#'   \item{dead}{the cow is dead (!)}
#'   \item{greedy}{the cow is greedy}
#'   \item{paranoid}{the cow is paranoid}
#'   \item{stoned}{the cow is stoned}
#'   \item{tired}{the cow is sleepy}
#'   \item{wired}{the cow has had too much caffeine}
#'   \item{young}{the cow in its younger days}
#' }
#' @docType data
cow.styles <- list(
    borg=list(eyes='=='),
    dead=list(eyes='XX', tongue='U'),
    greedy=list(eyes='$$'),
    paranoid=list(eyes='@@'),
    stoned=list(eyes='**', tongue='U'),
    tired=list(eyes='--'),
    wired=list(eyes='OO'),
    youthful=list(eyes='..')
)

construct.balloon <- function (message) {
}


