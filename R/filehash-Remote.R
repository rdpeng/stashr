######################################################################
## Class definitions

## Use 'http://' type URLs
setClass("filehashRemote",
         representation(url = "character",
                        dir = "character"),
         contains = "filehash"
         )

## For local directories
setClass("filehashLocal",
         representation(dir = "character"),
         contains = "filehash"
         )
         


######################################################################
## Methods for 'filehashLocal'

setMethod("dbInsert",
          signature(db = "filehashLocal", key = "character", value = "ANY"),
          function(db, key, value, overwrite=FALSE, ...) {
		if(file.exists(local.file.path(db,key)) & !overwrite){
                  stop("cannot overwrite previously saved file")
		}		
            ## save(value, file = local.file.path(db,key))
            con <- gzfile(local.file.path(db,key))
            open(con, "wb")
            on.exit(close(con), add=TRUE)
            serialize(value, con)
		#close(con)
		s <- unname(md5sum(local.file.path(db,key)))
		s2 <- paste(s,key,sep="  ")
		writeLines(s2, con = local.file.path.SIG(db,key))
		##update the 'keys' file
		if(!file.exists(file.path(db@dir, "keys"))) {
		      conA <- file(file.path(db@dir, "keys"))
            	open(conA, "a")
            	on.exit(close(conA), add=TRUE)
			cat(key, file = file.path(db@dir,"keys"),sep = "\n",
				append = TRUE)
		}
		else{	if(!dbExists(db,key)){
            	conA <- file(file.path(db@dir, "keys"))
            	open(conA, "a")
            	on.exit(close(conA), add=TRUE)
			cat(key, file = file.path(db@dir,"keys"),sep = "\n",
				append = TRUE)
			}
		}
          })

setMethod("dbFetch", signature(db = "filehashLocal", key = "character"),
          function(db, key, ...) {
		  if(!checkLocal(db,key)) 
			stop("Specified data does not exist") 
              read(db,key)
          })

setMethod("dbDelete", signature(db = "filehashLocal", key = "character"),
          function(db, key, ...){
		if(file.exists(local.file.path(db,key))) 
			file.remove(local.file.path(db,key))
		else stop("Specified file does not exist")
		if(file.exists(local.file.path.SIG(db,key))) 
			file.remove(local.file.path.SIG(db,key))
		else stop("Specified .SIG file does not exist")
		## Delete the key from the 'keys' file ##
		if(dbExists(db,key)) {	
			keylist <- dbList(db)
			keyindex <- match(key,keylist)
			newkeylist <- keylist[-keyindex]
			con <- file(file.path(db@dir, "keys"))
              	open(con, "wb")
              	on.exit(close(con))
			cat(newkeylist, file = file.path(db@dir,"keys"),sep = "\n")
		}
          })

setMethod("dbList", "filehashLocal",
          function(db, ...){
              con <- file(file.path(db@dir, "keys"))
              open(con, "rb")
              on.exit(close(con))
              readLines(con)
          })

setMethod("dbExists", signature(db = "filehashLocal", key = "character"),
          function(db, key, ...){
              key %in% dbList(db)	# returns a vector of T/F
          })


######################################################################
## Method definitions for 'filehashRemote'

setMethod("dbInsert",
          signature(db = "filehashRemote", key = "character", value = "ANY"),
          function(db, key, value, ...) {
              stop("cannot insert into a 'filehashRemote' database")
          })

setMethod("dbFetch", signature(db = "filehashRemote", key = "character"),
          function(db, key, offline = FALSE, ...){
              if(offline && !checkLocal(db,key)) stop("Haven't previously downloaded specified data,
				and you have set 'offline = TRUE'") 
		  if(!offline && !key%in%getlist(db)) stop("Specified data does not exist") 
              if(!offline && checkLocal(db,key)) 
              	{if(!md5sum(local.file.path(db,key)) 
                 		== scan(local.file.path.SIG(db,key),quiet=TRUE,what="character",sep=" ")[1])
                   getdata(db,key)
           	  	}
		  if(!checkLocal(db,key)) getdata(db,key)
              read(db,key)
          })

setMethod("dbDelete", signature(db = "filehashRemote", key = "character"),
          function(db, key, ...) {
              stop("cannot delete from a 'filehashRemote' database")
          })

setMethod("dbList", "filehashRemote",
          function(db, save=FALSE, ...){
              con <- url(file.path(db@url, "keys"))
              open(con, "rb")
              on.exit(close(con))
		  mylist <- readLines(con)
              if (save) cat(mylist, file = file.path(db@dir,"keys"),sep = "\n")
		  mylist
          })

setMethod("dbExists", signature(db = "filehashRemote", key = "character"),
          function(db, key, ...){
              key %in% getlist(db)	# returns a vector of T/F
          })


setGeneric("dbSync", function(db, ...) standardGeneric("dbSync"))

setMethod("dbSync", signature(db = "filehashRemote"),
          function(db, key = NULL, ...){
              if(!is.null(key) & !all(checkLocal(db,key))) 
                  stop("not all files referenced in the 'key' vector were 
				previously downloaded, no files updated")
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






########################################################################
## Prelim Functions  ###################################################
########################################################################

###############################
## local.file.path ############  Creates a file path in the local data  
###############################  directory (to be used internally).	

local.file.path <- function(db,key){
	file.path(db@dir,"data",key)
}

###############################
## local.file.path.SIG ######## Creates a file path in the local data  
############################### directory (to be used internally) for the SIG files.	

local.file.path.SIG <- function(db,key){
	file.path(db@dir,"data",paste(key,".SIG",sep=""))
}

#######################
## getlist ############ Reads the 'keys' file from the server. Has an option 
####################### to save the repository's list of keys in local dir.

getlist <- function(db){
	con <- url(file.path(db@url,"keys"))
	open(con, "rb")
	on.exit(close(con))
	readLines(con)
 	}

#################### Returns TRUE if data file for 'key' is in local dir, otherwise
## checkLocal ###### returns FALSE. We have 'key' allowed to be a character vector
#################### with more than one key.

checkLocal <- function(db, key){
    key %in% list.files(file.path(db@dir, "data")) # returns a vector of T/F
}


## Seven Functions #########################################################
############################################################################

####################
## 3) getdata ###### old 'fetch()', but now downloads the key & the SIG file
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
                      localFiles["data"], mode = "wb", cacheOK = FALSE)
        download.file(file.path(db@url, "data", paste(key, ".SIG", sep = "")),
                      localFiles["sig"], mode = "wb", cacheOK = FALSE)
    }, error = handler, interrupt = handler)

    if(inherits(status, "condition"))
        stop(gettextf("problem downloading data for key '%s': %s",
                      key, conditionMessage(cond)))
}

####################
## 4) read ######### Reads file associated with specified key from the local directory.
#################### Returns the data object associated with the key.

read <- function(db,key){
	if(!checkLocal(db,key)) stop("files associated with this key not yet downloaded")
	con <- gzfile(local.file.path(db,key))
	open(con, "rb")
	on.exit(close(con))
	unserialize(con) 
}

#################### Checks if the key exists in the local directory. If it doesn't, 
## 5) fetch ######## 'fetch' just downloads the data file & SIG file. If the key exists in  
#################### the local dir, then 'fetch' compares the SIG file in the local dir to the SIG
#################### file in the repository. If the SIGS are the same, 'fetch' reads the file from 
#################### the local dir and otherwise call getdata() to re-download the data
#################### file & SIG file. If 'offline=TRUE', 'fetch' skips the downloading step.
#################### The function returns the data object associated with the key.

library(tools)

fetch <- function(db, key, offline = FALSE){
		  if(offline && !checkLocal(db,key)) stop("Haven't previously downloaded specified data,
				and you have set 'offline = TRUE'") 
		  if(!offline && !key%in%getlist(db)) stop("Specified data does not exist") 
              if(!offline && checkLocal(db,key)) 
              	{if(!md5sum(local.file.path(db,key)) 
                 		== scan(local.file.path.SIG(db,key),quiet=TRUE,what="character",sep=" ")[1])
                   getdata(db,key)
           	  	}
		  if(!checkLocal(db,key)) getdata(db,key)
              read(db,key)
} 

#################### Updates all key/data pairs in the local directory by checking the
## 6) update ####### SIGs if 'key' is 'NULL'.  If 'key' is a character vector, then it
#################### only updates the specified key/data pairs (in which case, it first
#################### checks to ensure that all specified keys' files have been previously saved).

update <- function(db, key = NULL){
	if(!is.null(key) & !all(checkLocal(db,key))) 
		stop("not all files referenced in the 'key' vector were previously downloaded, no files updated")
	if(is.null(key)) 
		{list.local.files <- list.files(file.path(db@dir,"data"))
		key <- list.local.files[-grep(".SIG", list.local.files)]
		}
	for (i in key){ 
		if(!md5sum(local.file.path(db,i)) 
				== scan(local.file.path.SIG(db,i),quiet=TRUE,what="character",sep=" ")[1])
		getdata(db,i)
	}	
}

#################### Saves an R object ('value') as a file in the local directory.
## 7) insert ####### Can specify whether overwriting is OK. The new file is 
#################### associated with a specified key.

insert <- function(db,key,value,overwrite=FALSE){
	if(file.exists(local.file.path(db,key)) & !overwrite)
		stop("cannot overwrite previously saved file")
	else	{save(value, file = local.file.path(db,key))}
	}


