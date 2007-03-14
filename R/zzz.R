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
    stashROption("offline", FALSE)
}    

.stashROptions <- new.env()

## Valid options:
##
## quietDownload:  Should download progress be shown?
## offline:  Are we connected to the Internet [not yet implemented]

stashROption <- function(name, value) {
    if(missing(name))
        as.list(.stashROptions)
    else if(missing(value))
        get(name, .stashROptions, inherits = FALSE)
    else 
        assign(name, value, .stashROptions, inherits = FALSE)
}
