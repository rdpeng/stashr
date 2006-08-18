.onAttach <- function(lib, pkg) {
    dcf <- read.dcf(file.path(lib, pkg, "DESCRIPTION"))
    msg <- gettextf("%s (version %s)", dcf[, "Title"],
                    as.character(dcf[, "Version"]))
    writeLines(strwrap(msg))
}
