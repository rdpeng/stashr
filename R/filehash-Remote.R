setClass("filehashRemote",
         representation(url = "character",
                        dir = "character")
         )

setMethod("dbInsert", signature(db = "filehashRemote", key = "character", value = "ANY"),
          function(db, key, value) {
              stop("cannot insert into a 'filehashRemote' database")
          })

setMethod("dbFetch", signature(db = "filehashRemote", key = "character"),
          function(db, key) {

          })
              
setMethod("dbDelete", signature(db = "filehashRemote", key = "character"),
          function(db, key) {
              stop("cannot delete from a 'filehashRemote' database")
          })

setMethod("dbList", "filehashRemote",
          function(db) {

          })

setMethod("dbExists", signature(db = "filehashRemote", key = "character"),
          function(db, key) {

          })

setGeneric("dbSync", function(db, key) standardGeneric("dbSync"))

setMethod("dbSync", signature(db = "filehashRemote", key = "character"),
          function(db, key) {

          })
