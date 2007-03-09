## 'dbName' here should be a character vector with names 'url' and
## optionally 'dir', e.g. c(url = "http://....", dir = "/home/rpeng").

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

              if(!file.exists(versionFile(db)))
                  file.create(versionFile(db))
          })




