.onLoad <- function (libname, pkgname) {
    options('cowsay'=list(
                rude=FALSE,
                rude.cows=c('sodomized', 'sodomized-sheep', 'head-in')
            ))
}
