## 'dbName' here should be a character vector with names 'url' and
## optionally 'dir', e.g. c(url = "http://....", dir = "/home/rpeng").

## createRemote <- function(dbName) {
##     nms <- names(dbName)
## 
##     if(is.null(nms))
##         stop("'dbName' should have non-NULL names")
##     if(!("url" %in% nms))
##         stop("'dbName' should have a 'url' element")
##     URL <- dbName["url"]
## 
##     ## Check to see if 'url' is of http:// form.  If so, get 'dir'
##     ## element.  If 'url' is of file:// form, set the 'dir' element to
##     ## be the directory to which file:// points.
##     dir <- switch(checkURLtype(URL),
##                   http = {
##                       if(!("dir" %in% nms))
##                           stop("'dbName' should have a 'dir' element")
##                       else
##                           dbName["dir"]
##                   },
##                   file = sub("^file://", "", URL, fixed = TRUE, perl = TRUE)
##                   )    
##     create(URL, dir)
##     TRUE
## }

## 'dbName' should be the name of a directory

## initializeRemote <- function(dbName) {
##     r <- init(dbName)
## 
##     switch(checkURLtype(r$url),
##            http = {
##                new("remoteDB", url = r$url, dir = r$dir,
##                    name = basename(dbName))
##            },
##            file = {
##                new("localDB", url = r$url, dir = r$dir,
##                    name = basename(dbName))
##            })
## }
## 
## checkURLtype <- function(URL) {
##     if(length(grep("^http://", URL, fixed = TRUE, perl = TRUE)) > 0)
##         "http"
##     else if(length(grep("^file://", URL, fixed = TRUE, perl = TRUE)) > 0)
##         "file"
##     else
##         stop("'url' should be of type 'http://' or 'file://'")
## }


setMethod("initialize", "remoteDB",
          function(.Object, ...) {
              .Object <- callNextMethod()
              dbCreate(.Object)
              .Object
          })

setMethod("initialize", "localDB",
          function(.Object, ...) {
              .Object <- callNextMethod()
              dbCreate(.Object)
              .Object
          })

createLocalDir <- function(db) {
    ## create the local main directory and data sub-directory to
    ## store the data files ##
    datadir <- file.path(db@dir,"data")
    status <- dir.create(db@dir, showWarnings = FALSE, recursive = TRUE)
    
    if(!status && !file.exists(db@dir))
        stop(gettextf("problem creating directory '%s'", db@dir))
    status <- dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
    
    if(!status && !file.exists(datadir))
        stop(gettextf("problem creating directory '%s'", datadir))
}

setMethod("dbCreate",
          signature(db = "remoteDB"),
          function(db, ...) {
              ## remove trailing "/" on dir and url ##
              if (length(grep("/$", db@dir, perl = TRUE)) > 0)
                  db@dir <- sub("/$","", db@dir)
              if (length(grep("/$", db@url, perl = TRUE)) > 0)
                  db@url <- sub("/$","", db@url)

              createLocalDir(db)
 
              ## save url in the R workspace format in the main directory ##
              myurl <- db@url 
              save(myurl, file = file.path(db@dir,"url"))
          })

setMethod("dbCreate",
          signature(db = "localDB"),
          function(db, ...) {
              ## remove trailing "/" on dir ##
              if (length(grep("/$", db@dir, perl = TRUE)) > 0)
                  db@dir <- sub("/$","", db@dir)
              createLocalDir(db)
          })




