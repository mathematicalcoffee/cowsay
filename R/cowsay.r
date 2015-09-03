#' calls cowsay
#'
#' @param message {string} what to say
#' @template cow-features
#' @param cow {string} path to a cow file to use, or name of a pre-installed cow (see
#'             \code{\link{list.cows}})
#' @param wrap {integer} number of characters to wrap the message at (-1 not to wrap)
#' @param think {logical} is the cow thinking rather than saying?
#' @param style {string} a predefined style for the cow (e.g. 'dead' sets the cow's eyes
#'               to 'XX'). Overrides any eye string passed in.
#'               By default, no style is applied.
#'               See details for more styles.
#'
#' @details
#' The available cow styles are:
#'
#' \describe{
#'   \item{borg}{the cow looks like a Borg}
#'   \item{dead}{the cow is dead (!)}
#'   \item{default}{the default plain cow}
#'   \item{greedy}{the cow is greedy}
#'   \item{paranoid}{the cow is paranoid}
#'   \item{stoned}{the cow is stoned}
#'   \item{tired}{the cow is sleepy}
#'   \item{wired}{the cow has had too much caffeine}
#'   \item{young}{the cow in its younger days}
#' }
#' @export
#' @family cowsay
#' @return the cow-string, invisibly.
#' @examples
#' cowsay('mooooo')
#' cowsay('moooo', eyes='Oo')
cowsay <- function (message, cow='default', eyes='oo', tongue='  ', wrap=60,
                    think=F,
                    style=c('borg', 'dead', 'default', 'greedy', 'paranoid', 'stoned', 'tired', 'wired', 'young')) {


    if (!missing(style)) {
        style <- match.arg(style)
    } else {
        style <- NULL
    }

    if (!is.null(style)) {
        s <- cow.styles[[style]]
        if (!is.null(s$eyes)) eyes <- s$eyes
        if (!is.null(s$tongue)) tongue <- s$tongue
    }
    eyes <- format(substring(eyes, 1, 2), width=2)
    tongue <- format(substring(tongue, 1, 2), width=2)
    thoughts <- ifelse(think, 'o', '\\')

    # convert the cow into a path
    cowpath <- get.cowfile(cow)

    # get the cow.
    cowstring <- get.cow(cowpath, eyes=eyes, tongue=tongue, thoughts=thoughts)

    # get the speech bubble
    # we assume that if there are multiple elements of `message` they are each
    # on their own line.
    if (wrap > 0)
        message <- trim.message(message, width=wrap)
    messagestring <- construct.balloon(message, think=think)

    the_cow <- paste(messagestring, cowstring, sep='\n')

    message(the_cow, appendLF=FALSE)
    return(invisible(the_cow))
}

#' cowsay with a random cow.
#'
#' The cow, style, and think-style are all randomized (the eyes/tongue are not,
#'  but if the style gets randomized then that often specifies the eyes/tongue).
#'
#' If any of the arguments `cow`, `style` or `think` are given, this overrides
#'  the randomness for that argument.
#'
#' Note that if you supply `eyes` or `tongue`, the style overrides these if it
#'  has prespecified values for them.
#'
#' @inheritParams cowsay
#' @inheritParams list.cows
#' @param ... passed to \code{\link{cowsay}}.
#' @seealso \code{\link{cowsayOptions}} to change whether rude cows are included by default, and which cows are rude.
#' @family cowsay
#' @export
#' @examples
#' randomcowsay('MOOOOO')
randomcowsay <- function (message, cow=NULL, style=NULL, think=NULL, rude=cowsayOptions('rude'), ...) {
    if (is.null(cow)) cow <- sample(list.cows(rude=rude), 1)
    if (is.null(style)) style <- sample(names(cow.styles), 1)
    if (is.null(think)) think <- runif(1) < .5
    # TODO: random eyes, tongue?

    cowsay(message, cow=cow, style=style, think=think, ...)
}

#' Lists the currently-installed cows.
#'
#' This lists all the `*.rcow` files found under `path`.
#'
#' @param path {character vector|NULL} path(s) to list the cows under. If NULL,
#'              the cowpath is used (see \code{\link{get.cowpaths}}).
#' @param rude {boolean} whether to include "rude" cows or not.
#' @param ...  passed to \code{\link[base]{list.files}}.
#' @return a character vector of cow files found in the path.
#' @seealso \code{\link[base]{list.files}}; \code{\link{cowsayOptions}} to change whether rude cows are included by default, and which cows are rude.
#' @export
#' @family cow styles
list.cows <- function (path=NULL, rude=cowsayOptions('rude'), ...) {
    if (is.null(path)) path=get.cowpaths()
    cows <- list.files(path, pattern='*.rcow', ...)
    # exclude rude cows (rude cows don't have an extension)
    if (!rude) {
        rude.cows <- cowsayOptions('rude.cows')
        cows <- setdiff(cows, paste0(rude.cows, '.rcow'))
    }
    return(cows)
}

#' Cow styles (borg, dead, etc)
#'
#' The available styles are:
#'
#' \describe{
#'   \item{borg}{the cow looks like a Borg}
#'   \item{dead}{the cow is dead (!)}
#'   \item{default}{the default plain cow}
#'   \item{greedy}{the cow is greedy}
#'   \item{paranoid}{the cow is paranoid}
#'   \item{stoned}{the cow is stoned}
#'   \item{tired}{the cow is sleepy}
#'   \item{wired}{the cow has had too much caffeine}
#'   \item{young}{the cow in its younger days}
#' }
#'
#' The only differences here are in the eye and tongue configurations.
#' @docType data
#' @family cow styles
cow.styles <- list(
    borg=list(eyes='=='),
    dead=list(eyes='XX', tongue='U'),
    default=list(eyes='OO'),
    greedy=list(eyes='$$'),
    paranoid=list(eyes='@@'),
    stoned=list(eyes='**', tongue='U'),
    tired=list(eyes='--'),
    wired=list(eyes='OO'),
    young=list(eyes='..')
)

#' Looks for a particular cow.
#'
#' Looks for cows in this order:
#'
#' 1. If the input is a filename with extension '.rcow' and exists, we use it.
#' 2. Otherwise find the corresponding `{cow}.rcow` on the cowpath.
#'
#' Note that the `cow` should NOT have the extension
#'  (i.e. "three-eyes", not "three-eyes.cow")
#' @inheritParams cowsay
#' @return {character} the path to the first matching cow on the cowpath, throwing an error if not found.
#' @seealso \code{\link{get.cowpaths}} to see the cowpath.
#' @family cow styles
#' @examples
#' get.cowfile(system.file('cows/default.rcow', package='cowsay'))
#' get.cowfile('three-eyes')
#' \dontrun{
#' get.cowfile('nonexistentfoobarbaz')
#' }
#' @export
get.cowfile <- function (cow) {
    # 1. check if it's a path that exists and has extension rcow
    if (file.exists(cow) && grepl('\\.rcow$', cow)) {
        return(cow)
    }

    # strip extension
    cow <- sub('\\.rcow$', '', cow)

    # get search path
    paths <- get.cowpaths()

    # 2. Look for the rcow on that path, returning the first found.
    cowfile <- file.path(paths, paste0(cow, '.rcow'))
    if (any(file.exists(cowfile))) {
        # return first cow on path that matches
        return(cowfile[file.exists(cowfile)][1])
    }

    stop(sprintf("Could not find the '%s' cowfile!", cow))
}

#' Returns the search path for cows.
#'
#' * if environment variable `$COWPATH` is set, we use that.
#' * we always include the cows installed with this package.
#'
#' Order matters; if a particular cow is found in one of the earlier paths, we
#'  will not bother looking at the later paths.
#'
#' @return {character vector} directories that cows will be looked for under.
#' @family cow styles
#' @export
get.cowpaths <- function() {
    # 1. COWPATH
    paths <- Sys.getenv('COWPATH')
    if (paths != "") {
        paths <- strsplit(paths, .Platform$path.sep, fixed=T)[[1]]
    } else {
        paths <- NULL
    }

    # 2. System path
    paths <- c(paths, system.file('cows', package='cowsay'))

    return(paths)
}

#' Trims a message to a particular width.
#'
#' @inheritParams base::strwrap
#' @return {character vector} a vector of `x` chunked up into lines of approximately
#'   width `width`.
#'   If there were embedded newlines in `x`, the input is also split up according
#'   to this.
#' @family speech bubble functions
#' @seealso \code{\link[base]{strwrap}}
#' @examples
#' cowsay:::trim.message('I do not like green eggs and ham.\nI do not like them, Sam I Am!',
#'              width=10)
trim.message <- function (x, width=0.8 * getOption("width")) {
    x <- as.character(x)
    x <- x[!is.na(x)] # for some reason as.character(fortune) sometimes
                               # introduces these

    # in case there are embedded newlines...
    if (length(x) == 1)
        x <- strsplit(x, '\n')[[1]]

    return(strwrap(x, width=width))
}

#' Puts the message into the balloon
#' @param message {character vector} a character vector with the message to
#'         put into the bubble (one element per line).
#' @inheritParams cowsay
#' @return {string} a single string with the speech bubble in it (embedded newlines)
#' @family speech bubble functions
#' @details
#' We expect the message to be already wrapped to the appropriate number of
#'  lines.
#'
#' This places the message inside a speech/thought bubble.
#'
#' If it's a thought bubble, the sides of the bubble are `(` and `)`.
#'
#' If it's a speech bubble and one-line, the bubble boundaries are `<` and `>`.
#' If it's a speech bubble and more than one line, the top row has borders
#' `/` and `\`, the bottom `\` and `/`, and the middle rows `|` and `|`.
#'
#' (See the examples).
#' @examples
#' cat(cowsay:::construct.balloon('MOOOO', think=TRUE))
#' cat(cowsay:::construct.balloon('MOOOO', think=FALSE))
#' cat(cowsay:::construct.balloon(c('MOOOO', 'MOOO!!!'), think=FALSE))
construct.balloon <- function (message, think) {
    mlength <- max(nchar(message))
    format <- paste0("%s %-", mlength, "s %s")
    # determine the border elements.
    # if it's a thought bubble the sides of the bubble are ( and )
    # otherwise, if it's a one-line message the side parts of the bubble are < and >
    # otherwise (a normal speech-bubble), the top row has borders / and \,
    #  the middle rows have '|', and the bottom row has '\' and '/'
    # border is c(topleft, topright, left, right, bottomleft, bottomright)
    # e.g.::
    #
    # ( here is a      )   / here is a \
    # ( thought bubble )   | multiline |   < one-line speech bubble >
    #                      \ bubble    /
    n <- length(message)
    border.left <- '<'
    border.right <- '>'
    if (think) {
        border.left <- '('
        border.right <- ')'
    } else if (n > 1) {
        # message more than one line
        border.left <- c('/', rep('|', n - 2), '\\')
        border.right <- c('\\', rep('|', n - 2), '/')
    }

    # dumb multi-use of paste because if R not having nice strrep
    firstline <- paste0(' ', paste(rep('_', mlength + 2), collapse=''))
    lastline <- paste0(' ', paste(rep('-', mlength + 2), collapse=''))
    balloon <- paste0(c(firstline,
                        sprintf(format, border.left, message, border.right),
                        lastline),
                      collapse='\n')
    return(balloon)
}

#' Reads a cow from a cowfile, substituting the eyes, thoughts and tongue.
#'
#' Returns an error if the cow does not exist.
#' @template cow-features
#' @template cowfile
get.cow <- function (cowfile, eyes, thoughts, tongue) {
    if (!grepl('\\.rcow$', cowfile) || !file.exists(cowfile))
        stop(sprintf("The cowfile '%s' doesn't exist or doesn't have extension '.rcow'.", cowfile))
    cow <- read.cow.r(cowfile, eyes=eyes, thoughts=thoughts, tongue=tongue)
    return(cow)
}

#' Read an R-cow
#'
#' @template cow-features
#' @template cowfile
#' @details
#' R-cows consist of 1 (optionally 2) files:
#'
#' * `{cowname}.rcow`, which is an ASCII file consisting of the cow as-is,
#'     where lines starting with '#' are ignored as comments. If your cow has a
#'     legitimate line starting with has, indent the entire cow a space.
#'    In the file, the placeholders '$eyes', '$tongue' and '$thoughts' will be
#'    replaced by the user-provided eyes (2 characters), tongue (2 characters)
#'    and thoughts (1 character).
#' * (optional) `{cowname}.r`, an R file that (if it exists) is run BEFORE the
#'     rcow file is read in.
#'
#' Note that the parameter `cowfile` is the path to the ASCII cow (`{cowname}.rcow`).
#' Then if `{cowname}.r` exists, it will be run prior.
#'
#' The reason for having two separate files is if you decide instead that
#' R-cows should be R scripts, it's harder to define them purely because of
#' having to remember to escape backslashes, quotes, etc in the ASCII.
#'
#' This way, the cow art can be entirely unescaped, and the R script can
#'  include the R.
#' @examples
#' # three-eyes cow has .r and .rcow (.r file expands the eyes so it has 3)
#' cowfile <- system.file('cows', 'three-eyes.rcow', package='cowsay')
#' cat(cowsay:::read.cow.r(cowfile, eyes="..", thoughts="o", tongue="U"))
read.cow.r <- function (cowfile, eyes, thoughts, tongue) {
    # search for an R file of the same name
    rfile <- c(sub('\\.rcow$', '.r', cowfile),
              sub('\\.rcow$', '.R', cowfile))
    rfile <- rfile[file.exists(rfile)][1]
    # this file is not mandatory, just optional. so no errors if not exists
    if (!is.na(rfile)) {
        env <- new.env()
        env$eyes <- eyes
        env$thoughts <- thoughts
        env$tongue <- tongue
        env$gsubv <- gsubv
        # Catch errors, suppress warnings (?)
       suppressWarnings(tryCatch(source(rfile, local=env, verbose=F, echo=F, print.eval=F),
                        error=function (e) {
                            stop(sprintf("in `read.cow.r`:Error reading '%s': %s",
                                         rfile,
                                         e$message),
                                 call.=F)
                        }))

        # update values
        eyes <- env$eyes
        thoughts <- env$thoughts
        tongue <- env$tongue
    }

    # read the plain cow
    lines <- readLines(cowfile)
    lines <- lines[grep('^#', lines, invert=T)]
    # add newline at end
    if (!grepl('^\\s*$', lines[length(lines)])) lines=c(lines, '')
    cow <- paste(lines, collapse="\n")
    cow <- gsubv(c('$eyes', '$thoughts', '$tongue'),
                 c(eyes, thoughts, tongue),
                 cow, fixed=T)
    return(cow)
}
