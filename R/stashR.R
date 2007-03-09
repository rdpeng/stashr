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
######################################################################

setMethod("dbUnlink", signature(db = "localDB"),
          function(db, ...) {
              unlink(db@dir, recursive = TRUE)
          })

## new dbInsert ## (note: I removed overwrite option)

setMethod("dbInsert",
          signature(db = "localDB", key = "character", value = "ANY"),
          function(db, key, value, ...) {
              if(db@reposVersion != -1) 
                  stop("inserting keys into pervious versions not allowed")
                  
              ## update the 'version' file ##
              updateVersion(db,key)
              
              ## update the data files ##
              vn <- objectVersion(db,key) + 1
              
              con <- gzfile(local.file.path(db,key,vn), "wb")
              on.exit(close(con))
              serialize(value, con)

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
          function(db, key, ...) {
              if(db@reposVersion != -1) 
                  stop("deleting keys from previous versions not allowed")

              if(!(key %in% dbList(db)))
                  warning("specified key does not exist in current version")
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
                  else
                      character(0)
              }
          })

setMethod("dbExists", signature(db = "localDB", key = "character"),
          function(db, key, ...){
              key %in% dbList(db)	# returns a vector of T/F
          })


######################################################################
## Method definitions for 'remoteDB'

## currently ignoring (but maintaining) .SIG files and the associated
## utility functions readRemoteSIG, readLocalSIG and checkSIG
######################################################################


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


## Returns TRUE if a specific version of a key's file is included in the 
## relevant version of the repository stored on the specified URL

checkRemote<- function(db, key.v){
    info <- reposVersionInfo(db)
    if(length(info)!=0){
        keyFiles <- strsplit(info[length(info)], ":")[[1]][2]
        keyFilesSep <- strsplit(keyFiles," ")[[1]]
        key.v%in%keyFilesSep
    }
    else FALSE
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
          function(db, ...){
              info <- reposVersionInfo(db)
              if(length(info)!=0){
                  keyFiles <- strsplit(info[length(info)], ":")[[1]][2]
                  keyFilesSep <- strsplit(keyFiles," ")[[1]]
                  gsub("\\.[0-9]+$", "", keyFilesSep)
              }
          })

setMethod("dbExists", signature(db = "remoteDB", key = "character"),
          function(db, key, ...){
              key %in% dbList(db)  ## returns a vector of TRUE/FALSE
          })


setGeneric("dbSync", function(db, ...) standardGeneric("dbSync"))

setMethod("dbSync", signature(db = "remoteDB"),
          function(db, key = NULL, ...){
              ## only update if reposVersion = -1 ##
              if(db@reposVersion != -1)
                  stop("no synchronization for a 'remoteDB' object ",
                       "with a fixed version")
              if(!is.null(key) && !all(checkLocal(db,key))) 
                  stop("not all files referenced in the 'key' vector were ",
                       "previously downloaded, no files updated")

              if(is.null(key)) {
                  list.local.files <- list.files(file.path(db@dir, "data"),
                                                 full.names = TRUE,
                                                 all.files = TRUE)
                  ## exclude directories (causes error?)                      
                  use <- !file.info(list.local.files)$isdir  
                  list.local.files <- basename(list.local.files[use])
                  
                  dontuse <- grep("\\.SIG$", list.local.files)
                  key <- list.local.files[-dontuse]
              }
              
              ## see if the version of the repository has changed
              ## still needs work!
              
              for (i in key){ 
                  ##if(!checkSIG(db, i)) # ignore .SIG files for now
                  ## download new files when key is in latest repos version, but we 
                  ## don't the corresponding updated version of the key
                  if(!checkRemote(db,i) && dbExists(db, gsub("\\.[0-9]+","",i))){ 
                      ## load updated data #
                      getdata(db, gsub("\\.[0-9]+","",i))

                      ## delete the old data and .SIG file #
                      file.remove(file.path(db@dir,"data",i))
                      file.remove(file.path(db@dir,"data",paste(i,".SIG",sep="")))
                  }
                  ## delete files associated with keys that have been deleted ## 
                  if(!dbExists(db,gsub("\\.[0-9]+","",i))){
                      ## delete the old data and .SIG file #
                      file.remove(file.path(db@dir,"data",i))
                      file.remove(file.path(db@dir,"data",paste(i,".SIG",sep="")))
                  }	
              }	
          })




########################################################################
## Utility Functions  ##################################################
########################################################################

setGeneric("versionFile", function(x, ...) standardGeneric("versionFile"))

## Return the path for the 'version' file
setMethod("versionFile", "localDB",
          function(x, ...) {
              file.path(db@dir, "version")
          })

## Return the URL for the 'version' file
setMethod("versionFile", "remoteDB",
          function(x, ...) {
              file.path(db@url, "version")
          })
          

## use readLines to find last line, returns as character string
reposVersionInfo <- function(db){
    ## if(inherits(db, "localDB"))
    ##     verDir <- db@dir
    ## else
    ##     verDir <- db@url
    if((inherits(db, "localDB") && file.exists(versionFile(db)))
       || inherits(db, "remoteDB")) {  
        con <- file(versionFile(db), "r") ## 'version' is a text file
        on.exit(close(con))

        VerList <- readLines(con)
        rvn <- if(db@reposVersion == -1) 
            length(VerList)
        else
            db@reposVersion
        VerList[rvn]
    }
    else
        character(0)	
}

## returns "object version" associated with a given key

getKeyFiles <- function(db, key) {
    version <- readLines(versionFile(db))

    ## Strip repository version numbers
    v <- sub("^[0-9]+:", "", version)
    unlist(strsplit(vf, " ", fixed = TRUE))
}

objectVersion <- function(db, key){
    ## for localDB: 
    ## determine last version of the object in the repository    ##
    ## (note this object may have been previously deleted, so    ##
    ## we need to look in the data directory at the data files)  ##
    ## for remoteDB:
    ## read pertinent line of the version file from the internet ##
    if(inherits(db, "localDB")) {
        ## allFiles <- list.files(file.path(db@dir,"data"))
        ## keyFiles <- allFiles[-grep("\\.SIG$",allFiles)]	 ## returns character(0) if no files       
        ## o <- order(keyFiles[grep(paste("^",key,"\\.[0-9]+$",sep=""),keyFiles)],
        ##            decreasing = FALSE)
        ## oFiles <- keyFiles[o]

        keyFiles <- getKeyFiles(db, key)
        use <- grep(paste("^", key, "\\.[0-9]+$", sep=""), keyFiles)
        oFiles <- sort(keyFiles[use], decreasing = FALSE)
        latestFile <- oFiles[length(oFiles)]

        if(length(latestFile) != 0){
            latestFileSplit <- strsplit(latestFile,".", fixed = TRUE)[[1]]
            lastObjVer <- as.numeric(latestFileSplit[length(latestFileSplit)])
        }
        else
            lastObjVer <- 0
        lastObjVer
    }
    else { 
        info <- reposVersionInfo(db)

        if(length(info)!=0){
            keyFiles <- strsplit(info[length(info)], ":")[[1]][2]
            keyFilesSep <- strsplit(keyFiles," ")[[1]]
            v <- grep(paste("^",key,"\\.[0-9]+$",sep=""),keyFilesSep,value=TRUE)
            currNum <- as.numeric(substring(v,nchar(key)+2))
            if(length(currNum)==0) currNum <- 0
            currNum
        }
        else 0
    }
}




## function returning the integer corresponding to the lastest repos
## version by reading first integer of the last line of the version
## file

latestReposVersionNum <- function(db){ 
    info <- reposVersionInfo(db)
    if(length(info)!=0){
        as.numeric(strsplit(info[length(info)], ":")[[1]][1])
    }
    else 0
}


## reads last line of the version file creates new info on repository
## version to be inserted in last line of the version file updates
## keepKey = FALSE deletes the key from the version information


updatedReposVersionInfo <- function(db, key, keepKey = TRUE){
    ## find and remove key (for updating) from latest repository version info ##
    reposV <- latestReposVersionNum(db)+1
    info <- reposVersionInfo(db)
    if(length(info)!=0){
        keyFiles <- strsplit(info[length(info)], ":")[[1]][2]
        keyFilesSep <- strsplit(keyFiles," ")[[1]]
        v <- grep(paste("^",key,"\\.[0-9]+$",sep=""),keyFilesSep)

        if(length(v)==0)
            others <- keyFilesSep
        else
            others <- keyFilesSep[-v]
        if(length(others)==0)
            othersCollapsed <- paste(others, collapse=" ")
        else{	
            if(length(others)==1 & is.na(others[1]))
                othersCollapsed <- character(0)
            else 
                othersCollapsed <- paste(others, collapse=" ")
        }
        newKeyInfo <- paste(key,objectVersion(db,key)+1, sep=".")
        if(keepKey)
            updatedKeyFiles <- paste(othersCollapsed, newKeyInfo, sep=" ")
        else
            updatedKeyFiles <- othersCollapsed
    }
    else updatedKeyFiles <- paste(key,1,sep=".")
                                        # remove leading space resulting from intially inserting same key twice#
    updatedKeyFiles <- gsub("^ ","",updatedKeyFiles)
    paste(reposV, updatedKeyFiles, sep = ":") 
}



######### updating version file ########## 
updateVersion <- function(db,key, keepKey = TRUE){
    cat(updatedReposVersionInfo(db,key,keepKey),
        file = versionFile(db), sep = "\n", append = TRUE)
}


###############################
## local.file.path ############  Creates a file path in the local data  
###############################  directory (to be used internally).	

local.file.path <- function(db, key, objVerNum = objectVersion(db, key)){
    file.path(path.expand(db@dir), "data", paste(key,".",objVerNum,sep=""))
}

###############################
## local.file.path.SIG ######## Creates a file path in the local data  
############################### directory (to be used internally) for the SIG files.	

local.file.path.SIG <- function(db, key, objVerNum = objectVersion(db, key)){
    file.path(path.expand(db@dir), "data", paste(key,".",objVerNum,".SIG",sep=""))
}



#################### Returns TRUE if data file for 'key' is in local dir, otherwise
## checkLocal ###### returns FALSE. We have 'key' allowed to be a character vector
#################### with more than one key.

checkLocal <- function(db, key){
    ## key %in% list.files(file.path(db@dir, "data"), all.files = TRUE)
    datadir <- file.path(db@dir, "data")
    
    if(!file.exists(datadir))
        stop("local data directory does not exist")
    file.exists(local.file.path(db,key))      ## returns a vector of T/F
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
        remotePath <- file.path(db@url, "data",
                                paste(key, objectVersion(db, key), sep="."))
        download.file(remotePath, localFiles["data"], mode = "wb",
                      cacheOK = FALSE, quiet = stashROption("quietDownload"))

        remoteSIGPath <- file.path(db@url, "data",
                                   paste(key, objectVersion(db,key),"SIG",sep="."))
        download.file(remoteSIGPath, localFiles["sig"], mode = "wb",
                      cacheOK = FALSE, quiet = stashROption("quietDownload"))
    }, error = handler, interrupt = handler)

    if(inherits(status, "condition"))
        stop(gettextf("problem downloading data for key '%s': %s",
                      key, conditionMessage(status)))
}



####################
## read ############
## Reads file associated with specified key from the local directory.
#################### Returns the data object associated with the key.


read <- function(db, key){
    if(!checkLocal(db,key))
        stop(gettextf("files associated with key not yet downloaded", key))
    con <- gzfile(local.file.path(db,key), "rb")
    on.exit(close(con))
    unserialize(con) 
}


