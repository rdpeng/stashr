## 'dbName' here should be a character vector with names 'url' and
## optionally 'dir', e.g. c(url = "http://....", dir = "/home/rpeng").

createRemote <- function(dbName) {
    nms <- names(dbName)
    
    if(!("url" %in% nms))
        stop("'dbName' should have a 'url' element")
    URL <- dbName["url"]

    ## Check to see if 'url' is of http:// form.  If so, get 'dir'
    ## element.  If 'url' is of file:// form, set the 'dir' element to
    ## be the directory to which file:// points.
    dir <- switch(checkURLtype(URL),
                  http = {
                      if(!("dir" %in% nms))
                          stop("'dbName' should have a 'dir' element")
                      else
                          dbName["dir"]
                  },
                  file = sub("^file://", "", URL, fixed = TRUE, perl = TRUE)
                  )    
    create(URL, dir)
    TRUE
}

## 'dbName' should be the name of a directory

initializeRemote <- function(dbName) {
    r <- init(dbName)

    switch(checkURLtype(r$url),
           http = {
               new("filehashRemote", url = r$url, dir = r$dir,
                   name = basename(dbName))
           },
           file = {
               new("filehashLocal", url = r$url, dir = r$dir,
                   name = basename(dbName))
           })
}

checkURLtype <- function(URL) {
    if(length(grep("^http://", URL, fixed = TRUE, perl = TRUE)) > 0)
        "http"
    else if(length(grep("^file://", URL, fixed = TRUE, perl = TRUE)) > 0)
        "file"
    else
        stop("'url' should be of type 'http://' or 'file://'")
}
