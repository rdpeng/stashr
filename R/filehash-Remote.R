setClass("filehashRemote",
         representation(url = "character",
                        dir = "character")
         )

setMethod("dbInsert",
          signature(db = "filehashRemote", key = "character", value = "ANY"),
          function(db, key, value) {
              stop("cannot insert into a 'filehashRemote' database")
          })

setMethod("dbFetch", signature(db = "filehashRemote", key = "character"),
          function(db, key, offline = FALSE){
              if(!offline && checkLocal(db,key)) 
              {if(!md5sum(local.file.path(db,key)) 
                  == scan(local.file.path.SIG(db,key),quiet=TRUE,what="character",sep=" ")[1])
                   getdata(db,key)
           }
              read(db,key)
          })

setMethod("dbDelete", signature(db = "filehashRemote", key = "character"),
          function(db, key) {
              stop("cannot delete from a 'filehashRemote' database")
          })

setMethod("dbList", "filehashRemote",
          function(db, save=FALSE){
              con <- gzcon(url(file.path(db@url, "keys.gz")))
              open(con, "rb")
              on.exit(close(con))
              mylist <- readLines(con)
              if (save) save(mylist, file = file.path(db@dir,"list"))
              mylist 
          })

setMethod("dbExists", signature(db = "filehashRemote", key = "character"),
          function(db, key){
              key %in% getlist(db, save = FALSE)	# returns a vector of T/F
          })

setGeneric("dbSync", function(db, key) standardGeneric("dbSync"))

setMethod("dbSync", signature(db = "filehashRemote", key = "character"),
          function(db, key = NULL){
              if(!is.null(key) & !all(checkLocal(db,key))) 
                  stop("not all files referenced in the 'key' vector were previously downloaded, no files updated")
              if(is.null(key)) 
              {list.local.files <- list.files(file.path(db@dir, "data"))
               key <- list.local.files[-grep(".SIG", list.local.files)]
           }
              for (i in key){ 
                  if(!md5sum(local.file.path(db,i)) 
                     == scan(local.file.path.SIG(db,i),quiet=TRUE,what="character",sep=" ")[1])
                      getdata(db,i)
              }	
          })
