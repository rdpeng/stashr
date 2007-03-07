## function returning the integer corresponding to the lastest version
## of the specified key. Returns zero if new key.

## use readLines to find last line, returns as character string
reposVersionInfo <- function(db){
	if(file.exists(file.path(db@dir, "version"))){  
		con <- file(file.path(db@dir, "version"))
		open(con, "r")  ## 'version' is a text file
		on.exit(close(con))
		VerList <- readLines(con)
		ifelse(db@reposVersion == - 1, 
			rvn <- length(VerList), 
			rvn <- db@reposVersion)
		VerList[rvn]
	}
	else NULL	
}

## returns most recent "object version" associated with a given key

objectVersion <- function(db, key){ 
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


## BUG: need to read the key file version number from the 
## files corresponding to keys in data file instead of from 
## last line of the repository's version file
##(since if I remove all keys from current version and then
## re-insert data using an old key, as is, the numbering system
## starts up again at 1 and we lose the old version of the file

updatedReposVersionInfo <- function(db, key, keepKey = TRUE){ 
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
            file = file.path(dbLocal@dir,"version"),
            sep = "\n", append = TRUE)
}



###############################
## local.file.path ############  Creates a file path in the local data  
###############################  directory (to be used internally).	

local.file.path <- function(db,key,objVerNum=objectVersion(db, key)){
    	file.path(path.expand(db@dir), "data", paste(key,".",objVerNum,sep=""))
}

###############################
## local.file.path.SIG ######## Creates a file path in the local data  
############################### directory (to be used internally) for the SIG files.	

local.file.path.SIG <- function(db,key,objVerNum=objectVersion(db, key)){
	file.path(path.expand(db@dir), "data", paste(key,".",objVerNum,".SIG",sep=""))
}



## new dbInsert ## (note: I got rid of overwrite option)

setMethod("dbInsert",
          signature(db = "localDB", key = "character", value = "ANY"),
          function(db, key, value, ...) {

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
              
              ## update the 'version' file
              updateVersion(db,key)
              
          })


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

read <- function(db, key){
    if(!checkLocal(db,key))
        stop(gettextf("files associated with key '%s' not yet downloaded", key))
    con <- gzfile(local.file.path(db,key))
    open(con, "rb")
    on.exit(close(con))
    unserialize(con) 
}


setMethod("dbFetch", signature(db = "localDB", key = "character"),
          function(db, key, ...) {
              if(!checkLocal(db,key)) 
                  stop("specified data does not exist") 
              read(db, key)
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

## no longer delete files from repository, just delete key from latest line
## of the version file
setMethod("dbDelete", signature(db = "localDB", key = "character"),
          function(db, key, ...){
			if(!key%in%dbList(db)) warning("Specified key does not exist in current version")		  
 			updateVersion(db,key, keepKey = FALSE)
         })


############################################
## remote DB ###############################
############################################

## note dbInsert stays the same (doesn't work for remoteDB) ##

## currently ignoring dbSync and the associated utility functions
## readRemoteSIG, readLocalSIG and checkSIG


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
        download.file(file.path(db@url, "data", key, objectVersion(db,key)),
                      localFiles["data"], mode = "wb", cacheOK = FALSE,
                      quiet = .stashROptions$quietDownload)
        download.file(file.path(db@url, "data", paste(key, objectVersion(db,key), ".SIG", sep = "")),
                      localFiles["sig"], mode = "wb", cacheOK = FALSE,
                      quiet = .stashROptions$quietDownload)
    }, error = handler, interrupt = handler)

    if(inherits(status, "condition"))
        stop(gettextf("problem downloading data for key '%s': %s",
                      key, conditionMessage(status)))
}


setMethod("dbFetch", signature(db = "remoteDB", key = "character"),
          function(db, key, offline = FALSE, ...){
              if(offline && !checkLocal(db,key))
                  stop("have not previously downloaded specified data ", 
                       "and 'offline = TRUE'") 
              if(!offline && !(key %in% dbList(db)))
                  stop("specified key not in database")
		  # ignoring .SIG files for now ?	
              #if(!offline && checkLocal(db, key)) {
              #    ## Check the remote/local MD5 hash value
              #    if(!checkSIG(db, key))
              #        getdata(db,key)
              #}
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