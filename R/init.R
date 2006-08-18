createRemote <- function(dbName) {
    if(!all(c("url", "dir") %in% names(dbName)))
        stop("'dbName' should be a list with elements 'url' and 'dir'")
    with(dbName, create(url, dir))
    TRUE
}

initializeRemote <- function(dbName) {
    r <- init(dbName)
    new("filehashRemote", url = r$url, dir = r$dir)
}

