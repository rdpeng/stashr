.onAttach <- function(lib, pkg) {
    dcf <- read.dcf(file.path(lib, pkg, "DESCRIPTION"))
    msg <- gettextf("%s (%s %s)", dcf[, "Title"],
                    as.character(dcf[, "Version"]), dcf[, "Date"])
    message(paste(strwrap(msg), collapse = "\n"))    
}

.onLoad <- function(lib, pkg) {
    pkgList <- c("methods", "filehash")

    for(package in pkgList) {
        if(!require(package, quietly = TRUE, character.only = TRUE))
            stop(gettextf("'%s' package required", package))
    }
    if(!capabilities("http/ftp"))
        warning("'http/ftp' capabilities not available")
    stashROption("quietDownload", FALSE)
}    

.stashROptions <- new.env()

stashROption <- function(name, value) {
    if(missing(value))
        get(name, .stashROptions, inherits = FALSE)
    else
        assign(name, value, .stashROptions, inherits = FALSE)
}
