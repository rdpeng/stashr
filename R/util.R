setGeneric("setDir<-", function(db, value) standardGeneric("setDir<-"))

setReplaceMethod("setDir", signature(db = "remoteDB", value = "character"),
          function(db, value) {
              db@dir <- value
              db
          })
