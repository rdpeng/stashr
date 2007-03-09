setGeneric("setDir<-", function(db, value) standardGeneric("setDir<-"))

setReplaceMethod("setDir", signature(db = "remoteDB", value = "character"),
          function(db, value) {
              db@dir <- value
              db
          })

setGeneric("copyDB", function(db, ...) standardGeneric("copyDB"))

setMethod("copyDB", "localDB",
          function(db, dir, ...) {
              if(identical(dir, db@dir))
                  stop("cannot copy 'localDB' object to original directory")
              newdb <- new("localDB", dir = dir, name = db@name)
              keys <- dbList(db)

              for(key in keys) 
                  dbInsert(newdb, key, dbFetch(db, key))
              newdb
          })

getCurrentReposVersion <- function(db) {
    info <- reposVersionInfo(db)
    num <- strsplit(info, ":", fixed = TRUE)[[1]][1]
    as.numeric(num)
}

setGeneric("currentReposVersion",
           function(db, ...) standardGeneric("currentReposVersion"))

setMethod("currentReposVersion", "localDB",
          function(db, ...) {
              getCurrentReposVersion(db)
          })

setMethod("currentReposVersion", "remoteDB",
          function(db, ...) {
              getCurrentReposVersion(db)
          })

setGeneric("currentReposVersion<-",
           function(db, value) standardGeneric("currentReposVersion<-"))

setReplaceMethod("currentReposVersion",
                 signature(db = "remoteDB", value = "numeric"),
                 function(db, value) {
                     db@reposVersion <- value
                     db
                 })

setReplaceMethod("currentReposVersion",
                 signature(db = "localDB", value = "numeric"),
                 function(db, value) {
                     db@reposVersion <- value
                     db
                 })

convertOldStashR <- function() {
    infiles <- dir(".", all.files = TRUE)
    use <- !file.info(infiles)$isdir
    infiles <- infiles[use]
    outfiles <- sub("\\.SIG$", ".1.SIG", infiles)
    sigfiles <- grep("\\.SIG$", outfiles)
    outfiles[-sigfiles] <- paste(outfiles[-sigfiles], "1", sep = ".")

    for(i in seq(along = infiles)) {
        file.rename(infiles[i], outfiles[i])
    }
}
