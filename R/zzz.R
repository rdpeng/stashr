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
    
    ## Switch default filehash database type to 'Remote'
    ## filehashOption(defaultType = "Remote")

    ## Register 'filehashRemote' database format
    init <- list(create = createRemote, initialize = initializeRemote)
    registerFormatDB("Remote", init)
}    
