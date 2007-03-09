######################################################################
## Class definitions

## Use 'http://' type URLs
setClass("remoteDB",
         representation(url = "character",
                        dir = "character",
			reposVersion = "numeric"),
	 prototype = list(reposVersion = -1), 
         contains = "filehash"
         )

## For local directories
setClass("localDB",
         representation(dir = "character",
			reposVersion = "numeric"),
	 prototype = list(reposVersion = -1),
         contains = "filehash"
         )



######################################################################
## Methods for 'localDB'

setMethod("dbUnlink", signature(db = "localDB"),
          function(db, ...) {
              unlink(db@dir, recursive = TRUE)
          })

## new dbInsert ## (note: I removed overwrite option)

setMethod("dbInsert",
          signature(db = "localDB", key = "character", value = "ANY"),
          function(db, key, value, ...) {

              ## update the 'version' file ##
              updateVersion(db,key)

		  ## update the data files ##
	        vn <- objectVersion(db,key) + 1
	
              con <- gzfile(local.file.path(db,key,vn))
              open(con, "wb")
              
              tryCatch({
                  serialize(value, con)
              }, finally = {
                  if(isOpen(con))
                      close(con)
              })
              s <- unname(md5sum(local.file.path(db,key,vn)))
              s2 <- paste(s, key, vn, sep="  ")
              writeLines(s2, con = local.file.path.SIG(db,key,vn))
              

              
          })


setMethod("dbFetch", signature(db = "localDB", key = "character"),
          function(db, key, ...) {
              if(!checkLocal(db,key)) 
                  stop("specified data does not exist") 
              read(db, key)
          })

## doesn't delete files from repository, just deletes key from latest line
## of the version file
setMethod("dbDelete", signature(db = "localDB", key = "character"),
          function(db, key, ...){
			if(!key%in%dbList(db)) warning("Specified key does not exist in current version")		  
 			updateVersion(db,key, keepKey = FALSE)
         })


setMethod("dbList", "localDB",
          function(db, ...){
		info <- reposVersionInfo(db)
		if(length(info)!=0){
			keyFiles <- strsplit(info[length(info)], ":")[[1]][2]
			if(!is.na(keyFiles)){
				keyFilesSep <- strsplit(keyFiles," ")[[1]]
				gsub("\\.[0-9]+$", "", keyFilesSep)
			}
			else character(0)
		}
          })

setMethod("dbExists", signature(db = "localDB", key = "character"),
          function(db, key, ...){
              key %in% dbList(db)	# returns a vector of T/F
          })


######################################################################
## Method definitions for 'remoteDB'

setMethod("dbInsert",
          signature(db = "remoteDB", key = "character", value = "ANY"),
          function(db, key, value, ...) {
              stop("cannot insert into a 'remoteDB' database")
          })

readRemoteSIG <- function(db, key) {
    con <- url(file.path(db@url, "data", paste(key, "SIG", sep = ".")))
    open(con, "r")  ## SIG files are text
    on.exit(close(con))

    val <- scan(con, quiet = TRUE, what = "character", sep = " ")[1]
    as.character(val)
}

readLocalSIG <- function(db, key) {
    path <- local.file.path.SIG(db, key)
    val <- scan(path, quiet = TRUE, what = "character", sep = " ")[1]
    as.character(val)
}


## Return TRUE if local and remote SIGs are the same; FALSE otherwise

checkSIG <- function(db, key) {
    localSIG <- readLocalSIG(db, key)
    remoteSIG <- readRemoteSIG(db, key)

    isTRUE(localSIG == remoteSIG)
}
                       

setMethod("dbFetch", signature(db = "remoteDB", key = "character"),
          function(db, key, offline = FALSE, ...){
              if(offline && !checkLocal(db,key))
                  stop("have not previously downloaded specified data ", 
                       "and 'offline = TRUE'") 
              if(!offline && !(key %in% dbList(db)))
                  stop("specified key not in database")
		  # ignoring .SIG files for now 			
              #if(!offline && checkLocal(db, key)) {
              #    ## Check the remote/local MD5 hash value
              #    if(!checkSIG(db, key))
              #        getdata(db,key)
              #}
              if(!checkLocal(db, key))	## downloads new key's files if key version has changed
                  getdata(db,key)
              read(db, key)
          })

setMethod("dbDelete", signature(db = "remoteDB", key = "character"),
          function(db, key, ...) {
              stop("cannot delete from a 'remoteDB' database")
          })

setMethod("dbList", "remoteDB",
          function(db, save = FALSE, ...){
              con <- url(file.path(db@url, "keys"))
              mylist <- tryCatch({
                  open(con, "r")  ## 'keys' file is text
                  readLines(con)
              }, error = function(err) {
                  character(0)
              }, finally = {
                  if(isOpen(con))
                      close(con)
              })
              if (save)
                  cat(mylist, file = file.path(db@dir,"keys"),sep = "\n")
              mylist
          })

setMethod("dbExists", signature(db = "remoteDB", key = "character"),
          function(db, key, ...){
              key %in% dbList(db)  ## returns a vector of TRUE/FALSE
          })


setGeneric("dbSync", function(db, ...) standardGeneric("dbSync"))

setMethod("dbSync", signature(db = "remoteDB"),
          function(db, key = NULL, ...){
              if(!is.null(key) && !all(checkLocal(db,key))) 
                  stop("not all files referenced in the 'key' vector were ",
                       "previously downloaded, no files updated")
              if(is.null(key)) {
                  list.local.files <- list.files(file.path(db@dir, "data"),
                                                 all.files = TRUE)
                  use <- !file.info(list.local.files)$isdir  ## exclude directories
                  list.local.files <- list.local.files[use]

                  dontuse <- grep(".SIG", list.local.files, fixed = TRUE)
                  key <- list.local.files[-dontuse]
              }
              for (i in key){ 
                  if(!checkSIG(db, i))
                      getdata(db,i)
              }	
          })




########################################################################
## Prelim Functions  ###################################################
########################################################################

###############################
## local.file.path ############  Creates a file path in the local data  
###############################  directory (to be used internally).	

local.file.path <- function(db,key){
    file.path(path.expand(db@dir), "data", key)
}

###############################
## local.file.path.SIG ######## Creates a file path in the local data  
############################### directory (to be used internally) for the SIG files.	

local.file.path.SIG <- function(db,key){
    file.path(path.expand(db@dir), "data", paste(key,".SIG",sep=""))
}


#################### Returns TRUE if data file for 'key' is in local dir, otherwise
## checkLocal ###### returns FALSE. We have 'key' allowed to be a character vector
#################### with more than one key.

checkLocal <- function(db, key){
    ## key %in% list.files(file.path(db@dir, "data"), all.files = TRUE)
    datadir <- file.path(db@dir, "data")

    if(!file.exists(datadir))
        stop("local data directory does not exist")
    file.exists(file.path(datadir, key))      ## returns a vector of T/F
}



####################
## getdata ######### downloads the key & the SIG file
####################

getdata <- function(db,key){
    localFiles <- c(data = local.file.path(db, key),
                    sig = local.file.path.SIG(db, key))

    handler <- function(cond) {
        ## If a condition is thrown (e.g. error or interrupt), delete
        ## whatever was downloaded
        ex <- file.exists(localFiles)
        file.remove(localFiles[ex])
        cond
    }
    status <- tryCatch({
        download.file(file.path(db@url, "data", key),
                      localFiles["data"], mode = "wb", cacheOK = FALSE,
                      quiet = .stashROptions$quietDownload)
        download.file(file.path(db@url, "data", paste(key, ".SIG", sep = "")),
                      localFiles["sig"], mode = "wb", cacheOK = FALSE,
                      quiet = .stashROptions$quietDownload)
    }, error = handler, interrupt = handler)

    if(inherits(status, "condition"))
        stop(gettextf("problem downloading data for key '%s': %s",
                      key, conditionMessage(status)))
}

####################
## read ############ Reads file associated with specified key from the local directory.
#################### Returns the data object associated with the key.

read <- function(db, key){
    if(!checkLocal(db,key))
        stop(gettextf("files associated with key '%s' not yet downloaded", key))
    con <- gzfile(local.file.path(db,key))
    open(con, "rb")
    on.exit(close(con))
    unserialize(con) 
}




