#' calls cowsay
#'
#' @param message {string} what to say
#' @template cow-features
#' @param cow {string} path to a cow file to use, or name of a pre-installed cow (see
#'             \code{\link{list.cows}})
#' @param wrap {integer} number of characters to wrap the message at (-1 not to wrap)
#'             Note that if not explicity specified, this will set `wrap=-1`
#'             if the input `message` has a specific `print` method (so that, for
#'             example, `cowsay`-ing the results of a t-test will not wrap and
#'             mess up the output).
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
#' 
#' # let's cowsay some more interesting things.
#' Note this doesn't wrap even though the length might exceed your `getOption('wrap')`.
#'  This is done automagically to avoid messed up output.
#' cowsay(head(mtcars))
#' # if `wrap` is explicity provided, we can override that.
#' cowsay(head(mtcars), wrap=60)
#'
#' \dontrun{
#' # every .Rprofile should have this..
#' library(fortunes)
#' cowsay(fortune())
#' }
cowsay <- function (message, cow='default', eyes='oo', tongue='  ', wrap=getOption('width'),
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
    if (is.null(cowpath)) {
        stop(sprintf("Could not find the '%s' cowfile!", cow))
    }
    
    # get the cow.
    cowstring <- get.cow(cowpath, eyes=eyes, tongue=tongue, thoughts=thoughts)

    # get the speech bubble
    # we assume that if there are multiple elements of `message` they are each
    # on their own line.
    if (wrap > 0)
        message <- get.message(message, store.print.method=TRUE, width=wrap)
    else
        message <- get.message(message, store.print.method=TRUE)
    if (!is.null(attr(message, 'print.method'))) {
        if (missing(wrap)) wrap <- -1
        attr(message, 'print.method') <- NULL
    }
    if (wrap > 0)
        message <- wrap.message(message, width=wrap)
    messagestring <- construct.balloon(message, think=think)

    the_cow <- paste(messagestring, cowstring, sep='\n')
    attr(the_cow, 'cowtype') <- attr(cowstring, 'cowtype')
    
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
#' @param ... passed to \code{\link{cowsay}}.
#' @family cowsay
#' @export
#' @examples
#' randomcowsay('MOOOOO')
randomcowsay <- function (message, cow=NULL, style=NULL, think=NULL, ...) {
    if (is.null(cow)) cow <- sample(list.cows(), 1)
    if (is.null(style)) style <- sample(names(cow.styles), 1)
    if (is.null(think)) think <- runif(1) < .5
    # TODO: random eyes, tongue?

    cowsay(message, cow=cow, style=style, think=think, ...)
}

#' Lists the currently-installed cows.
#'
#' This lists all the `*.cow`, `*.rcow` files found under `path`.
#'
#' @param path {character vector|NULL} path(s) to list the cows under. If NULL,
#'              the cowpath is used (see \code{\link{get.cowpaths}}).
#' @param ...  passed to \code{\link[base]{list.files}}.
#' @return a character vector of cow files found in the path.
#' @seealso \code{\link[base]{list.files}}
#' @export
#' @family cow styles
list.cows <- function (path=NULL, ...) {
    if (is.null(path)) path=get.cowpaths()
    list.files(path, pattern='*.r?cow', ...)
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
#' 1. If the input is a filename with extension 'cow' or 'rcow' and exists, we use it;
#' 2. The corresponding R cow (`{cow}.rcow`) in the cowpath in order;
#' 3. a Perl/plain cow with extension '.cow' (e.g. you downloaded a cow pack
#'     and have not ported them to rcows or plaincows yet)
#'
#' Note that the `cow` should NOT have the extension
#'  (i.e. "three-eyes", not "three-eyes.cow")
#' @inheritParams cowsay
#' @return {character} the path to the first matching cow on the cowpath, or
#'  NULL if not found.
#' @family cow styles
#' @examples
#' get.cowfile('default')
#' get.cowfile('three-eyes')
#' @export
get.cowfile <- function (cow) {
    # 0 check if it's a path that exists and has extension r?cow
    if (file.exists(cow) && grepl('\\.r?cow$', cow)) {
        return(cow)
    }
    # strip extension
    cow <- sub('\\.r?cow$', '', cow)

    # get search path
    paths <- get.cowpaths()

    # 1. look for an rcow
    cowfile <- file.path(paths, paste0(cow, '.rcow'))
    if (any(file.exists(cowfile))) {
        # return first cow on path that matches
        return(cowfile[file.exists(cowfile)][1])
    }
    
    # 2. look for a Perl cow
    cowfile <- file.path(paths, paste0(cow, '.cow'))
    if (any(file.exists(cowfile))) {
        # return first cow on path that matches
        return(cowfile[file.exists(cowfile)][1])
    }

    return(NULL)
}

#' Path to cows if you install from the repositories
.sys.cowpath <- '/usr/share/cowsay/cows'

#' Returns the search path for cows.
#'
#' * if environment variable `$COWPATH` is set, we use that.
#' * we always include the cows installed with this package.
#' * we do NOT include '/usr/share/cowsay/cows'.
#'
#' Order matters; if a particular cow is found in one of the earlier paths, we
#'  will not bother looking at the later paths (subject to the rules in
#'  \code{\link{get.cowfile}}: Rcows are preferred over Perl cows).
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

    # 3. '/usr/share/cowsay/cows' (if exists)
    #if (isTRUE(file.info(.sys.cowpath)$isdir)) {
    #    paths <- c(paths, .sys.cowpath)
    #}

    return(paths)
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

#' Reads a cow from a cowfile.
#' @template cow-features
#' @template cowfile
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
        attr(cow, 'cowtype') <- 'R'

    # 2. If it's a Perl cow, read it in as such
    } else if (is.perl.cow(cowfile)) {
        PERL <- Sys.which('perl')[1]
        if (PERL != '') {
            cow <- read.cow.perl(cowfile, eyes=eyes, thoughts=thoughts, tongue=tongue)
            attr(cow, 'cowtype') <- 'perl'
        } else {
            cow <- read.cow.noperl(cowfile, eyes=eyes, thoughts=thoughts, tongue=tongue)
            attr(cow, 'cowtype') <- 'noperl'
        }

    # 3. Otherwise, it's a plain cow.
    } else {
        cow <- read.cow.plain(cowfile, eyes=eyes, thoughts=thoughts, tongue=tongue)
        attr(cow, 'cowtype') <- 'plain'
    }

    return(cow)
}

#' Is a cow a Perl-cow?
#' A cow is a perl cow if `$the_cow` can be found in the file (rudimentary check!) and the extension is '.cow'.
#' @inheritParams get.cow
#' @family cowfile parsing
#' @return {boolean} whether the cow is a Perl cow or not.
is.perl.cow <- function (cowfile) {
    isperlcow <- file.exists(cowfile)
    if (!isperlcow) return(isperlcow)
    isperlcow <- grepl('\\.cow$', cowfile, ignore.case=T)
    if (!isperlcow) return(isperlcow)
    isperlcow <- isperlcow && length(grep('$the_cow', readLines(cowfile), fixed=T))
    return(isperlcow)
}

#' Reads an R-cow.
#'
#' @template cow-features
#' @template cowfile
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
#' @examples
#' # three-eyes cow has .r and .rcow (.r file expands the eyes so it has 3)
#' cowfile <- system.file('cows', 'three-eyes.rcow', package='cowsay')
#' cat(cowsay:::read.cow.r(cowfile, eyes="..", thoughts="o", tongue="U"))
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
    cow <- read.cow.plain(cowfile, eyes, thoughts, tongue) 
    return(cow)
}


#' Read a plain-text cow
#'
#' Reads a cowfile where we take the cow as-is (comment lines allowed), do
#'  a straight substitution of eyes/tongue/thoughts tokens.
#' @template cow-features
#' @template cowfile
#' @details
#' The cow file:
#'
#' * can have comments. ANY line starting with `#` is a comment and is ignored.
#'   (if your cow has a line starting with `#`, indent the entire cow one space).
#' * can use the placeholders '$eyes', '$tongue', '$thoughts'; these are replaced
#' * every non-comment line is just the ASCII cow as-is (i.e. no `the_cow = `
#'     required).
#' @examples
#' # this cow happens to be a simple one (no further code required)
#' cowfile <- system.file('cows', 'small.rcow', package='cowsay')
#' cat(cowsay:::read.cow.plain(cowfile, eyes="..", thoughts="o", tongue="U"))
read.cow.plain <- function (cowfile, eyes, thoughts, tongue) {
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

#' Read a cowfile (original Perl format)
#'
#' These are cow files from the original cowsay distribution, written in Perl.
#' We run them through a perl interpreter (if you have one installed); if you
#' don't have Perl installed we throw an error.
#' @template cow-features
#' @template cowfile
#' @param perl {string}: path to the Perl executable (by default
#'         `Sys.which('perl')[1]`)
#' @examples
#' \dontrun{
#' # if you have the original cowsay installed on your system...
#' cowfile <- '/usr/share/cowsay/cows/three-eyes.cow'
#' cat(cowsay:::read.cow.perl(cowfile, eyes="Oo", thoughts="\\", tongue="U"))
#' #         \  ^___^
#' #          \ (Ooo)\_______
#' #            (___)\       )\/\
#' #             U  ||----w |
#' #                 ||     ||
#' }
# To get around escaping problems:
# http://stackoverflow.com/questions/16632223/executing-perl-from-r-perlquote-shquote
# TODO: mistake in perl file?
read.cow.perl <- function (cowfile, eyes, thoughts, tongue, perl=Sys.which('perl')[1]) {
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
            stop(sprintf("The resulting cow was empty; error in the cowfile '%s'?", cowfile))
        } else {
            # add newline at end
            if (!grepl('^\\s*$', res[length(res)])) res=c(res, '')
            res <- paste(res, collapse="\n")
        }
  }
  return(res)
}

#' Reads a Perl-style cowfile where Perl is not installed (crude substitutions)
#'
#' @template cow-features
#' @template cowfile
#' @details
#' If you are trying to read in a Perl-style cowfile but Perl is not installed,
#' this tries to read it in and use some EXTREMELY rudimentary regex to parse
#' the cow:
#'
#' 1. Strip the `$the_cow = <<"EOC";` and `EOC` lines if they are there
#' 2. Replace `$eyes`, `$tongue` and `$thoughts` with the appropriate values
#'
#' Any lines before `$the_cow << EOC` and after the ending `EOC` are ignored.
#'
#' Everything else (including any other code) is read verbatim! So you really
#' don't want to be in the situation where you're using this function.
read.cow.noperl <- function (cowfile, eyes, thoughts, tongue) {
    lines <- readLines(cowfile)
    thecow <- grep('\\$the_cow *= *<< *', lines)
    if (length(thecow)) {
        cowline <- lines[thecow[1]]
        # trim out everything up to & including the '$the_cow << EOC;' line.
        lines <- lines[(thecow[1] + 1):length(lines)]

        # trim out everything after & including the final 'EOC' line.
        m <- regexpr('\\$the_cow *= *<< *"?([A-Za-z_]+)"? *; *$', cowline, perl=T)
        st <- attr(m, 'capture.start')
        if (m != -1) {
            EOC <- substr(cowline, st, st + attr(m, 'capture.length') - 1)
            endline <- grep(paste0('^', EOC, ' *$'), lines)
            if (length(endline)) {
                lines <- lines[1:(endline-1)]
            }
        }
    }
    # add newline at end
    if (!grepl('^\\s*$', lines[length(lines)])) lines=c(lines, '')
    cow <- paste(lines, collapse="\n")
    cow <- gsubv(c('$eyes', '$thoughts', '$tongue'),
                 c(eyes, thoughts, tongue),
                 cow, fixed=T)    
    return(cow)
}
