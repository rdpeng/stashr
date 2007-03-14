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
                  stop("inserting key into pervious version not allowed")
                  
              ## update the data files ##
              vn <- objectVersion(db,key) + 1
              
              ## update the 'version' file ##
              updateVersion(db,key)
              
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
                  stop(gettextf("key '%s' not in database", key))
              read(db, key)
          })

## doesn't delete files from repository, just deletes key from latest line
## of the version file
setMethod("dbDelete", signature(db = "localDB", key = "character"),
          function(db, key, ...) {
              if(db@reposVersion != -1) 
                  stop("deleting key from previous version not allowed")
              if(!(key %in% dbList(db)))
                  stop(gettextf("key '%s' not in current version", key))
              
              updateVersion(db,key, keepKey = FALSE)
          })


setMethod("dbList", "localDB",
          function(db, ...){
              info <- reposVersionInfo(db)

              if(length(info)!=0){
                  keyFiles <- strsplit(info, ":")[[1]][2]

                  if(!is.na(keyFiles)){
                      keyFilesSep <- strsplit(keyFiles," ")[[1]]
                      gsub("\\.[0-9]+$", "", keyFilesSep)
                  }
                  else
                      character(0)
              }
              else
                  character(0)
          })

setMethod("dbExists", signature(db = "localDB", key = "character"),
          function(db, key, ...){
              key %in% dbList(db)	# returns a vector of T/F
          })

## db <- new("localDB",dir="testlocal",name="test")

## db <- new("remoteDB",url="http://www.biostat.jhsph.edu/~seckel/remoteDBExampleVersioning", dir="remotelocal",name="remote")



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

    if(length(info) != 0){
        keyFiles <- strsplit(info, ":", fixed = TRUE)[[1]][2]
        keyFilesSep <- strsplit(keyFiles, " ", fixed = TRUE)[[1]]
        key.v %in% keyFilesSep
    }
    else
        FALSE
}

outOfDate <- function(db, key) {
    info <- reposVersionInfo(db)
    objectVersion(db, key)
}

setMethod("dbFetch", signature(db = "remoteDB", key = "character"),
          function(db, key, ...){
              if(!(key %in% dbList(db)))
                  stop(gettextf("key '%s' not in database", key))

              ## downloads new key's files if key version has changed
              if(!checkLocal(db, key))	
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

              if(length(info) != 0){
                  keyFiles <- strsplit(info, ":")[[1]][2]
                  keyFilesSep <- strsplit(keyFiles," ", fixed = TRUE)[[1]]

                  ## Strip the object version numbers off
                  gsub("\\.[0-9]+$", "", keyFilesSep)
              }
              else
                  character(0)
          })

setMethod("dbExists", signature(db = "remoteDB", key = "character"),
          function(db, key, ...){
              key %in% dbList(db)  ## returns a vector of TRUE/FALSE
          })

stripVersion <- function(key.v) {
    gsub("\\.[0-9]+", "", key.v)
}

setGeneric("dbSync", function(db, ...) standardGeneric("dbSync"))

setMethod("dbSync", signature(db = "remoteDB"),
          function(db, key = NULL, ...){
              ## only update if reposVersion = -1 ##
              if(db@reposVersion != -1)
                  stop("no synchronization for a 'remoteDB' object ",
                       "with a fixed version")
              if(!is.null(key) && !all(checkLocal(db, key))) 
                  stop("not all files referenced in the 'key' vector were ",
                       "previously downloaded, no files updated")
              
              list.local.files <- list.files(file.path(db@dir, "data"),
                                             full.names = TRUE, all.files = TRUE)
              ## exclude directories (causes error?)                      
              use <- !file.info(list.local.files)$isdir  
              list.local.files <- basename(list.local.files[use])
              dontuse <- grep("\\.SIG$", list.local.files)
              list.local.files <- list.local.files[-dontuse]

              key <- if(is.null(key)) 
                  list.local.files
              else {
                  use <- stripVersion(list.local.files) %in% key
                  list.local.files[use]
              }
              ## see if the version of the repository has changed
              ## still needs work!
              
              for (i in key){ 
                  ## delete files associated with keys that have been deleted ## 
                  if(!dbExists(db, stripVersion(i))) {
                      ## delete the old data and .SIG file #
                      file.remove(file.path(db@dir, "data", i),
                                  file.path(db@dir, "data",
                                            paste(i, "SIG", sep = ".")))
                  }
                  else {  ## key exists in database
                      ## download new files when key is in latest
                      ## repos version, but we don't the corresponding
                      ## updated version of the key
                      if(!checkRemote(db, i)) { 
                          ## load updated data #
                          getdata(db, stripVersion(i))
                          
                          ## delete the old data and .SIG file #
                          file.remove(file.path(db@dir, "data", i),
                                      file.path(db@dir, "data",
                                                paste(i, "SIG", sep = ".")))
                      }
                  }
              }	
          })




########################################################################
## Utility Functions  ##################################################
########################################################################

setGeneric("versionFile", function(db, ...) standardGeneric("versionFile"))

## Return the path for the 'version' file
setMethod("versionFile", "localDB",
          function(db, ...) {
              file.path(db@dir, "version")
          })

## Return the URL for the 'version' file
setMethod("versionFile", "remoteDB",
          function(db, ...) {
              file.path(db@url, "version")
          })
          

## use readLines to find last line or the line corresponding to
## 'db@reposVersion', returns as character string

readVersionFileLine <- function(db) {
    ## Note:  'file' takes complete URLs as well as file paths
    con <- file(versionFile(db), "r") ## 'version' is a text file
    on.exit(close(con))
                  
    VerList <- readLines(con)

    if(length(VerList) > 0) {
        rvn <- if(db@reposVersion == -1) 
            length(VerList)
        else
            db@reposVersion
        VerList[rvn]
    }
    else
        character(0)
}

setGeneric("reposVersionInfo",
           function(db, ...) standardGeneric("reposVersionInfo"))

setMethod("reposVersionInfo", "localDB",
          function(db, ...) {
              if(file.exists(versionFile(db))) 
                  readVersionFileLine(db)
              else
                  character(0)
          })

cacheVersionFile <- function(db) {
    rpath <- versionFile(db)
    download.file(rpath, file.path(db@dir, "version"), mode = "w",
                  cacheOK = FALSE, quiet = TRUE)
}

setMethod("reposVersionInfo", "remoteDB",
          function(db, ...) {
              cacheVersionFile(db)
              readVersionFileLine(db)
          })

setAs("remoteDB", "localDB",
      function(from) {
          new("localDB", dir = from@dir, name = from@name)
      })

## 'getKeyFiles' uses the 'version' file instead of reading the 'data'
## directory directly

## returns "object version" associated with a given key
## 
## for localDB: 
## determine last version of the object in the repository    ##
## (note this object may have been previously deleted, so    ##
## we need to look in the data directory at the data files)  ##
##
## for remoteDB:
## read pertinent line of the version file from the internet ##

getKeyFiles <- function(db) {
    version <- readLines(versionFile(db))

    ## Strip repository version numbers
    v <- sub("^[0-9]+:", "", version)
    keyFiles <- unlist(strsplit(v, " ", fixed = TRUE), use.names = FALSE)

    if(is.null(keyFiles))
        keyFiles <- character(0)
    keyFiles
}

sortByVersionNumber <- function(keyFiles) {
    splitFiles <- strsplit(keyFiles, ".", fixed = TRUE)

    ## The object version number is always at the end
    num <- sapply(splitFiles, function(x) x[length(x)])
    num <- as.numeric(num)
    keyFiles[order(num, decreasing = FALSE)]
}


## Get the version number for an object corresponding to
## 'db@reposVersion'

getSpecificObjectVersion <- function(db, key) {
    info <- reposVersionInfo(db)
    currNum <- 0
    
    if(length(info) != 0){
        keyFiles <- strsplit(info, ":")[[1]][2]
        keyFilesSep <- strsplit(keyFiles, " ", fixed = TRUE)[[1]]
        v <- grep(paste("^", key, "\\.[0-9]+$", sep = ""),
                  keyFilesSep, value = TRUE)
        currNum <- if(length(v) > 0)
            as.numeric(substring(v, nchar(key) + 2))
        else
            0
    }
    currNum
}

## For 'db@reposVersion == -1' in a 'localDB', figure out the latest
## version number for an object

calculateLatestObjectVersion <- function(db, key) {
    keyFiles <- getKeyFiles(db)

    use <- grep(paste("^", key, "\\.[0-9]+$", sep=""), keyFiles)
    oFiles <- sortByVersionNumber(keyFiles[use])
    latestFile <- oFiles[length(oFiles)]
    
    if(length(latestFile) != 0){
        latestFileSplit <- strsplit(latestFile,".", fixed=TRUE)[[1]]
        lastObjVer <- latestFileSplit[length(latestFileSplit)]
        as.numeric(lastObjVer)
    }
    else
        0
}

setGeneric("objectVersion", function(db, ...) standardGeneric("objectVersion"))

setMethod("objectVersion", "localDB",
          function(db, key, ...) {
              if(db@reposVersion == -1)
                  sapply(key, function(x) calculateLatestObjectVersion(db, x))
              else 
                  sapply(key, function(x) getSpecificObjectVersion(db, x))
          })

setMethod("objectVersion", "remoteDB",
          function(db, key, ...) {
              sapply(key, function(x) getSpecificObjectVersion(db, x))
          })




## function returning the integer corresponding to the lastest repos
## version by reading first integer of the last line of the version
## file

latestReposVersionNum <- function(db){ 
    info <- reposVersionInfo(db)
    if(length(info)!=0){
        as.numeric(strsplit(info, ":")[[1]][1])
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
        keyFiles <- strsplit(info, ":")[[1]][2]
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
    else
        updatedKeyFiles <- paste(key,1,sep=".")
    ## remove leading space resulting from intially inserting same key twice#
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

local.file.path <- function(db, key, objVerNum = objectVersion(db, key)) {
    file.path(db@dir, "data", paste(key, objVerNum, sep="."))
}

###############################
## local.file.path.SIG ######## Creates a file path in the local data  
############################### directory (to be used internally) for the SIG files.	

local.file.path.SIG <- function(db, key, objVerNum = objectVersion(db, key)){
    file.path(db@dir, "data", paste(key, objVerNum, "SIG", sep = "."))
}



#################### Returns TRUE if data file for 'key' is in local dir, otherwise
## checkLocal ###### returns FALSE. We have 'key' allowed to be a character vector
#################### with more than one key.

checkLocal <- function(db, key){
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
        stop(gettextf("data for key '%s' not yet downloaded", key))
    con <- gzfile(local.file.path(db, key), "rb")
    on.exit(close(con))
    unserialize(con) 
}


