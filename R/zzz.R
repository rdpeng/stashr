.onAttach <- function(lib, pkg) {
    dcf <- read.dcf(file.path(lib, pkg, "DESCRIPTION"))
    msg <- gettextf("%s (version %s %s)", dcf[, "Title"],
                    as.character(dcf[, "Version"]), dcf[, "Date"])
    writeLines(strwrap(msg))
}

.onLoad <- function(lib, pkg) {
    pkgList <- c("methods", "filehash")

    for(package in pkgList) {
        if(!require(package, quietly = TRUE, character.only = TRUE))
            stop(gettextf("'%s' package required", package))
    }
    if(!capabilities("http/ftp"))
        warning("'http/ftp' capabilities not available")
    ## .stashROptions$quietDownload <- FALSE
    stashROption("quietDownload", FALSE)
}    

.stashROptions <- new.env()

stashROption <- function(name, value) {
    assign(name, value, .stashROptions)
}
