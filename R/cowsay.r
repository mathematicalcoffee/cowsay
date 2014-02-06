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
cowsay <- function (message, cow='default', eyes='oo', tongue='  ', wrap=60,
                    think=F,
                    # TODO: a better way to do the below.
                    style=c('borg', 'dead', 'greedy', 'paranoid', 'stoned', 'tired', 'wired', 'young')) {


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
    thoughts <- ifelse(think, 'o', '\\')
    
    # convert the cow into a path
    cowpath <- get.cowpath(cow)
    
    # get the cow.
    cowstring <- get.cow(cowpath, eyes=eyes, tongue=tongue, thoughts=thoughts)

    # get the speech bubble
    # we assume that if there are multiple elements of `message` they are each
    # on their own line.
    if (wrap > 0)
        message <- trim.message(message, width=wrap)
    messagestring <- construct.balloon(message, think=think)

    the_cow <- paste(messagestring, cowstring, sep='\n')
    
    message(the_cow)
    return(invisible(the_cow))
}

# Hmm. where shall we look for cows?
# TODO UPTO
get.cowpath <- function (cow) {
    has.ext <- grepl('\\.r?cow$', cow)
    
    if (!has.ext) cow <- paste0(cow, c('.rcow', '.cow'))
        
    candidates <- cow
    # look in system.file('cows', package='cowsay')
    # look in /usr/share/cowsay/cows?
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
    list.files(path, pattern='*.r?cow', ...)
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

trim.message <- function (text, width=0.8 * getOption("width")) {
    text <- as.character(text)
    text <- text[!is.na(text)] # for some reason as.character(fortune) sometimes
                               # introduces these
    
    # in case there are embedded newlines...
    if (length(text) == 1)
        text <- strsplit(text, '\n')[[1]]
    
    return(strwrap(text, width=width))
}

# TODO: assume message is trimmed?
# We expect the message to be a number of lines already.
# TODO: for fortunes, you need to trim the message.
construct.balloon <- function (msg, think) {   
    mlength <- max(nchar(msg))
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
    n <- length(msg)
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
                        sprintf(format, border.left, msg, border.right),
                        lastline),
                      collapse='\n')
    return(balloon)
}

get.cow <- function (cowpath, eyes, thoughts, tongue) {
    cow <- -1
    # determine what sort of cow it is. and R cow (.rcow), perl cow (.cow with 
    #  $the_cow) or plain cow.
    if (grepl('\\.rcow$', cowpath)) {
        # 1. try R
        cow <- read.cow.r(cowpath, eyes=eyes, thoughts=thoughts, tongue=tongue,
                          throw.error=T)
        
        # since R cows are explicitly defined as such (.rcow), throw an error if
        # they don't parse right
        if (cow == -1) {
            stop("Error reading the cow.\nIt is an R cow, so make sure `the_cow` is set.")
        }
    }
    # TODO: convert all my perl cows to R ones because by default I shouldn't be
    # reading in perl cows all the time, I should be reading in R cows.

    # 2. try Perl. We won't try to explicitly detect this (would require loading
    #    the file just to grep for $the_cow in it) (??)
    # TODO: give a warning if it was a perl-cow and we don't have perl
    PERL <- Sys.which('perl')
    if (PERL != '') {
        cow <- read.cow.perl(cowpath, eyes=eyes, thoughts=thoughts, tongue=tongue, perl=PERL)
    }
    
    # 3. try reading it in verbatim, removing the `$the_cow << EOC;` and `EOC` lines (if any)
    if (cow == -1) {    
        cow <- read.cow.plain(cowpath)
    }
    return(cow)
}

#'@ return {string|int} the cow string, or -1 if there was some error.
read.cow.r <- function (cowfile, eyes, thoughts, tongue) {
    env <- new.env()
    env$eyes <- eyes
    env$thoughts <- thoughts
    env$tongue <- tongue
    env$gsubv <- gsubv
    # Catch errors, suppress warnings (?)
    cow <- suppressWarnings(tryCatch(source(cowpath, local=env, verbose=F, echo=F, print.eval=F),
                    error = function (e) e))
    # return -1 on an error (TODO: throw an error, must be invalid R)
    if (inherits(cow, 'error')) {
        cow <- -1        
    # we expect the_cow to have been populated in the R file.
    } else if (exists('the_cow', env)) {
        cow <- env$the_cow
    # however if it wasn't we'll take the last value (provided it was a string)
    } else if (length(cow$value) > 0 && is.character(cow$value) && nchar(cow$value) > 0) {
        cow <- cow$value
    # must have not assigned `the_cow` and the last value of the script was not
    # cow-like.        
    } else {
        cow <- -1
    }
    return(cow)
}

# To get around escaping problems:
# http://stackoverflow.com/questions/16632223/executing-perl-from-r-perlquote-shquote
#' @param cowfile {string}: path to the cowfile
#' @param eyes {string}: eye string, e.g. "oo"
#' @param thoughts {string}: used in drawing the speech bubble, e.g. "o" or "\\"
#' @param tongue {string}: the tongue string, e.g. "U"
#' @return -1 if an error occured, or a character string with the cow (with embedded newlines)
#' @example
#' cowfile <- system.file('cows', 'three-eyes.cow', package='cowsay')
#' cat(read.cow.perl(cowfile, eyes="Oo", thoughts="\\", tongue="U"))
#' #         \  ^___^
#' #          \ (Ooo)\_______
#' #            (___)\       )\/\
#' #             U  ||----w |
#' #                 ||     ||
read.cow.perl <- function (cowfile, eyes, thoughts, tongue, perl=Sys.which('perl')) {
    res <- -1
    if (perl != '') {
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
            res <- -1
        } else {
            res <- paste(res, collapse="\n")
        }
  }
  return(res)
}

#' reads a cow file as plain text with naive stripping of Perl parts
#' 
#' @param cowfile
#' @param eyes
#' @param thoughts
#' @param tongue
#'
#' This can be used to try construct a cow where:
#' 
#' * it is a Perl cow file, but the user doesn't have Perl
#' * it is simply a plain-text cow file (with `$eyes`, `$tongue` and `$thoughts`
#'   in the appropriate places)
#'
#' It:
#' 
#' 1. strips the `$the_cow <<"EOC";` and `EOC` lines if they are there
#' 2. replaces `$eyes`, `$tongue` and `$thoughts` with the appropriate values
#' 
#' Everything else (including any other code) is read verbatim!
#' (TODO: COMMENTS up the top permitted?)
read.cow.plain <- function (cowfile) {
    lines <- readLines(cowfile)
    thecow <- grep('\\$the_cow *<< *', lines)
    if (length(thecow)) {
        cowline <- lines[thecow[1]]
        # trim out everything up to & including the '$the_cow << EOC;' line.
        lines <- lines[(thecow[1] + 1):length(lines)]

        # trim out everything after & including the final 'EOC' line.
        m <- regexpr('\\$the_cow *<< *"?([A-Za-z_]+)"? *; *$', cowline, perl=T)
        st <- attr(m, 'capture.start')
        if (m != -1) {
            EOC <- substr(cowline, st, st + attr(m, 'capture.length') - 1)
            endline <- grep(paste0('^', EOC, ' *$'), lines)
            if (length(endline)) {
                lines <- lines[1:endline]
            }
        }
    }
    cow <- paste(lines, collapse="\n")
    cow <- gsubv(c('$eyes', '$thoughts', '$tongue'),
                 c(eyes, thoughts, tongue),
                 cow, fixed=T)    
    return(cow)
}
