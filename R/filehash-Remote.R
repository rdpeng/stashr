setClass("filehashRemote",
         representation(url = "character",
                        dir = "character")
         )

setGeneric("dbInsert", function(db, key, value) standardGeneric("dbInsert"))

setMethod("dbInsert",
          signature(db = "filehashRemote", key = "character", value = "ANY"),
          function(db, key, value) {
              stop("cannot insert into a 'filehashRemote' database")
          })

setGeneric("dbFetch", function(db, key, offline) standardGeneric("dbFetch"))

setMethod("dbFetch", signature(db = "filehashRemote", key = "character"),
          function(db, key, offline = FALSE){
              if(!offline && checkLocal(db,key)) 
              {if(!md5sum(local.file.path(db,key)) 
                  == scan(local.file.path.SIG(db,key),quiet=TRUE,what="character",sep=" ")[1])
                   getdata(db,key)
           }
              read(db,key)
          })

setGeneric("dbDelete", function(db, key) standardGeneric("dbDelete"))

setMethod("dbDelete", signature(db = "filehashRemote", key = "character"),
          function(db, key) {
              stop("cannot delete from a 'filehashRemote' database")
          })

setGeneric("dbList", function(db, save) standardGeneric("dbList"))

setMethod("dbList", "filehashRemote",
          function(db, save=FALSE){
              con <- gzcon(url(file.path(db@url, "keys.gz")))
              open(con, "rb")
              on.exit(close(con))
              mylist <- readLines(con)
              if (save) save(mylist, file = file.path(db@dir,"list"))
              mylist 
          })

setGeneric("dbExists", function(db, key) standardGeneric("dbExists"))

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

getlist <- function(db, save=FALSE){
	con <- gzcon(url(file.path(db@url,"keys.gz")))
	open(con, "rb")
	on.exit(close(con))
	mylist <- readLines(con)
	if (save) save(mylist, file = file.path(db@dir,"list"))
	mylist 
	}

#################### Returns TRUE if data file for 'key' is in local dir, otherwise
## checkLocal ###### returns FALSE. We have 'key' allowed to be a character vector
#################### with more than one key.

checkLocal <- function(db, key){
	key %in% list.files(file.path(db@dir,"data")) # returns a vector of T/F
}

## Seven Functions #########################################################
############################################################################

####################
## 1) create #######
####################

create <- function(myurl,dir){	
	## remove trailing "/" on dir and myurl ##
	if (length(grep("/$",list(dir),perl=T))==1) dir <- sub("/$","", dir)
	if (length(grep("/$",list(myurl),perl=T))==1) myurl <- sub("/$","", myurl)
	## create the local main directory and data sub-directory to store the data files ##
	dir.create(dir)
	dir.create(file.path(dir,"data"))
	## save myurl in the R workspace format in the main directory ## 
	save(myurl, file = file.path(dir,"url"))
	}

####################
## 2) init #########
####################

init <- function(dir){
	## remove trailing "/" on dir ##
	if (length(grep("/$",list(dir),perl=T))==1) dir <- sub("/$","", dir)
	load(file.path(dir,"url"))
	list(url=myurl,dir=dir)
	}

####################
## 3) getdata ###### old 'fetch()', but now downloads the key & the SIG file
####################

getdata <- function(db,key){
	download.file(file.path(db@url,"db",key), local.file.path(db,key), mode="wb", cacheOK=FALSE)
	download.file(file.path(db@url,"db",paste(key,".SIG",sep="")), local.file.path.SIG(db,key), mode="wb", cacheOK=FALSE)
}

####################
## 4) read ######### Reads file associated with specified key from the local directory.
#################### Returns the data object associated with the key.

read <- function(db,key){
	if(checkLocal(db,key)==F) stop("files associated with this key not yet downloaded")
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
	if(offline==FALSE & checkLocal(db,key)) 
		{if(!md5sum(local.file.path(db,key)) 
				== scan(local.file.path.SIG(db,key),quiet=TRUE,what="character",sep=" ")[1])
			getdata(db,key)
		}
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
	if(file.exists(local.file.path(db,key)) & overwrite==FALSE)
		stop("cannot overwrite previously saved file")
	else	{save(value, file = local.file.path(db,key))}
	}


