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
