# SCOOPED! https://github.com/sckott/cowsay

#' calls cowsay
#'
#' @param message: what to say
#' @param cow: path to a cow file to use, or name of a pre-installed cow (see
#'             \code{\link{list.cows}})
#' @param eyes: characters to use for the eyes
#'              (only the first two characters are used), e.g. "oo"
#' @param tongue: characters to use for the tongue
#'                (at most the first two characters are used), e.g. "U", "\/"
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

# ---- below here, done.
 
#' Lists the currently-installed cows.
#'
#' This lists all the `*.cow`, `*.rcow` files found under `path`.
#'
#' @param path: the directory to list all cows under, default being those
#'              installed in the cowsay package.
#' @param ...: passed to \code{\link[base]{list.files}}.
#' @return a character vector of cow files installed with the cowsay package.
#' @seealso \code{\link[base]{list.files}}
#' @export
#' @family cow styles
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
#'
#' The only differences here are in the eye and tongue configurations.
#' @docType data
#' @family cow styles
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

#TODO: Sys.getenv('COWPATH') and split on colon!!
#' Looks for a particular cow.
#'
#' Looks for cows in this order:
#'
#' 1. The corresponding R cow (`{cow}.rcow`) in the package files
#' 2. In `/usr/share/cowsay/cows` for a Perl cow (will only be here if you have
#'     the original cowsay installed).
#'
#' Note that the `cow` should NOT have the extension
#'  (i.e. "three-eyes", not "three-eyes.cow")
#' @inheritParams cowsay
#' @return
#' @family cow styles
#' @examples
#' get.cowpath('default')
#' get.cowpath('three-eyes')
#' @export
get.cowpath <- function (cow) {
    # strip extension
    cow = sub('\\.r?cow$', '', cow)

    # 1. look for an rcow
    cowfile = system.file('cows', paste0(cow, '.rcow'), package='cowsay')
    if (file.exists(cowfile))
        return(cowfile)

    # 2. look for a Perl cow
    cowfile = file.path
    has.ext <- grepl('\\.r?cow$', cow)
    
    if (!has.ext) cow <- paste0(cow, c('.rcow', '.cow'))
        
    candidates <- cow
    # look in system.file('cows', package='cowsay')
    # look in /usr/share/cowsay/cows?
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
#' trim.message('I do not like green eggs and ham.\nI do not like them, Sam I Am!',
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
#' @inheritParams cowsay
#' @param message {character vector} a character vector with the message to
#'         put into the bubble (one element per line).
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
#' cat(construct.balloon('MOOOO', think=T))
#' cat(construct.balloon('MOOOO', think=F))
#' cat(construct.balloon(c('MOOOO', 'MOOO!!!'), think=F))
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

    # 2. If it's a Perl cow, read it in as such
    } else if (is.perl.cow(cowfile)) {
        PERL <- Sys.which('perl')
        if (PERL != '')
            cow <- read.cow.perl(cowfile, eyes=eyes, thoughts=thoughts, tongue=tongue)
        else
            cow <- read.cow.noperl(cowfile, eyes=eyes, thoughts=thoughts, tongue=tongue)

    # 3. Otherwise, it's a plain cow.
    } else {
        cow <- read.cow.plain(cowfile, eyes=eyes, thoughts=thoughts, tongue=tongue)
    }

    return(cow)
}

#' Is a cow a Perl-cow?
#' A cow is a perl cow if `$the_cow` can be found in the file (rudimentary check).
#' @inheritParams get.cow
#' @family cowfile parsing
#' @return {boolean} whether the cow is a Perl cow or not.
is.perl.cow = function (cowfile) {
    isperlcow = file.exists(cowfile)
    if (!isperlcow) return(isperlcow)
    isperlcow = isperlcow && length(grep('$the_cow', readLines(cowfile), fixed=T))
    return(isperlcow)
}

#' Reads an R-cow.
#'
#' @template cowr
#' @details
#' R-cows consist of 1 (optionally 2) files:
#'
#' * `{cowname}.rcow`, which is an ASCII file consisting of the cow as-is,
#'     where comments (lines starting with `#`) are ignored - just like a plain cow.
#' * (optional) `{cowname}.r`, an R file that (if it exists) is run BEFORE the
#'     rcow file is read in.
#'
#' Note that the parameter `cowfile` is the path to the ASCII cow (`{cowname}.rcow`).
#' Then if `{cowname}.r` exists, it will be run prior.
#'
#' The reason of having two separate parts is if you decide instead that
#' R-cows should be R scripts, it's harder to define them purely because of
#' having to remember to escape backslashes, quotes, etc in the ASCII.
#'
#' This way, the plain cow part can be entirely unescaped, and the R script can
#'  include the R.
#' @example
#' three-eyes cow has .r and .rcow (.r file expands the eyes so it has 3)
#' cowfile <- system.file('cows', 'three-eyes.rcow', package='cowsay')
#' cat(read.cow.r(cowfile, eyes="..", thoughts="o", tongue="U"))
#' TODO test
read.cow.r <- function (cowfile, eyes, thoughts, tongue) {
    # search for an R file of the same name
    rfile <- c(sub('\\.r?cow$', '.r', cowfile),
              sub('\\.r?cow$', '.R', cowfile))
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
                        error = function (e) {
                            stop(sprintf("in `read.cow.r`:Error reading '%s': %s",
                                         rfile,
                                         e$message),
                                 call.=F)
                        }))
    }
    # update values
    eyes <- env$eyes
    thoughts <- env$thoughts
    tongue <- env$tongue

    # read the plain cow
    cow <- read.cow.plain(cowfile, eyes, thoughts, tongue) 
    return(cow)
}


#' Read a plain-text cow
#'
#' Reads a cowfile where we take the cow as-is (comment lines allowed), do
#'  a straight substitution of eyes/tongue/thoughts tokens.
#' @template cowr
#' @details
#' The cow file:
#'
#' * can have comments. ANY line starting with `#` is a comment and is ignored.
#'   (if your cow has a line starting with `#`, indent the entire cow one space).
#' * can use the placeholders '$eyes', '$tongue', '$thoughts'; these are replaced
#' * every non-comment line is just the ASCII cow as-is (i.e. no `the_cow = `
#'     required).
#' @example
#' # this cow happens to be a simple one (no further code required)
#' cowfile <- system.file('cows', 'small.rcow', package='cowsay')
#' cat(read.cow.plain(cowfile, eyes="..", thoughts="o", tongue="U"))
#' # TODO
#'
read.cow.plain <- function (cowfile, eyes, thoughts, tongue) {
    lines <- readLines(cowfile)
    lines <- lines[grep('^#', lines, inv=T)]
    cow <- paste(lines, collapse="\n") 
    cow <- gsubv(c('$eyes', '$thoughts', '$tongue'),
                 c(eyes, thoughts, tongue),
                 cow, fixed=T)    
    return(cow)
}

#' Read a cowfile (original Perl format)
#'
#' These are cow files from the original cowsay distribution, written in Perl.
#' We run them through a perl interpreter (if you have one installed); if you
#' don't have Perl installed we throw an error.
#' @template cowr
#' @example
#' \dontrun{
#' # if you have the original cowsay installed on your system...
#' cowfile <- '/usr/share/cowsay/cows/three-eye.cow'
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
#' 1. Strip the `$the_cow <<"EOC";` and `EOC` lines if they are there
#' 2. Replace `$eyes`, `$tongue` and `$thoughts` with the appropriate values
#'
#' Any lines before `$the_cow << EOC` and after the ending `EOC` are ignored.
#'
#' Everything else (including any other code) is read verbatim! So you really
#' don't want to be in the situation where you're using this function.
read.cow.noperl <- function (cowfile, eyes, thoughts, tongue) {
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
