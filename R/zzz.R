.onAttach <- function(lib, pkg) {
    dcf <- read.dcf(file.path(lib, pkg, "DESCRIPTION"))
    msg <- gettextf("%s (version %s)", dcf[, "Title"],
                    as.character(dcf[, "Version"]))
    writeLines(strwrap(msg))
}

.onLoad <- function(lib, pkg) {
    pkgList <- c("methods", "filehash", "tools")

    for(package in pkgList) {
        if(!require(package, quietly = TRUE, character.only = TRUE))
            stop(gettextf("'%s' package required", package))
    }
    if(!capabilities("http/ftp"))
        warning("'http/ftp' capabilities not available")
    
    ## Register 'filehashRemote' database format
    init <- list(create = createRemote, initialize = initializeRemote)
    registerFormatDB("Remote", init)
}    
