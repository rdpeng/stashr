## function returning the integer corresponding to the lastest version
## of the specified key. Returns zero if new key.

## use readLines to find last line, returns as character string
reposVersionInfo <- function(db){
	if(class(db)=="localDB")
		verDir <- db@dir
	else  verDir <- db@url
	if({class(db)=="localDB" & file.exists(file.path(verDir, "version"))}
		| class(db)=="remoteDB"){  
		con <- file(file.path(verDir, "version"))
		open(con, "r")  ## 'version' is a text file
		on.exit(close(con))
		VerList <- readLines(con)
		ifelse(db@reposVersion == - 1, 
			rvn <- length(VerList), 
			rvn <- db@reposVersion)
		VerList[rvn]
	}
	else character(0)	
}

## returns "object version" associated with a given key

objectVersion <- function(db, key){
	## for localDB: 
	## determine last version of the object in the repository 	 ##
	## (note this object may have been previously deleted, so    ##
	## we need to look in the data directory at the data files)  ##
	## for remoteDB:
	## read pertinent line of the version file from the internet ##
	if(class(db)=="localDB"){
		allFiles <- list.files(file.path(db@dir,"data"))
		keyFiles <- allFiles[-grep("\\.SIG$",allFiles)]	## returns character(0) if no files
		o <- order(keyFiles[grep(paste("^",key,"\\.[0-9]+$",sep=""),keyFiles)],decreasing=FALSE)
		oFiles <- keyFiles[o]
		latestFile <-oFiles[length(oFiles)]
		if(length(latestFile)!=0){
			latestFileSplit <- strsplit(latestFile,"\\.")[[1]]
			lastObjVer <- as.numeric(latestFileSplit[length(latestFileSplit)])
		}
		else lastObjVer <- 0
		lastObjVer
	}
	else{ 
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
            file = file.path(db@dir,"version"),
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
        download.file(file.path(db@url, "data", paste(key, objectVersion(db,key),sep=".")),
                      localFiles["data"], mode = "wb", cacheOK = FALSE )#,
                      #quiet = .stashROptions$quietDownload)		## I commented these out because they were causing an error
        download.file(file.path(db@url, "data", paste(key, objectVersion(db,key), "SIG", sep = ".")),
                      localFiles["sig"], mode = "wb", cacheOK = FALSE )#,
                      #quiet = .stashROptions$quietDownload)
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


checkRemote<- function(db, key.v){
		info <- reposVersionInfo(db)
		if(length(info)!=0){
			keyFiles <- strsplit(info[length(info)], ":")[[1]][2]
			keyFilesSep <- strsplit(keyFiles," ")[[1]]
			key.v%in%keyFilesSep
		}
		else FALSE
}



setGeneric("dbSync", function(db, ...) standardGeneric("dbSync"))

setMethod("dbSync", signature(db = "remoteDB"),
          function(db, key = NULL, ...){
		## only update if reposVersion = -1 ##
		if(db@reposVersion == -1){

              if(!is.null(key) && !all(checkLocal(db,key))) 
                  stop("not all files referenced in the 'key' vector were ",
                       "previously downloaded, no files updated")
              if(is.null(key)) {
                  list.local.files <- list.files(file.path(db@dir, "data"),
                                                 all.files = FALSE)
                 # use <- !file.info(list.local.files)$isdir  ## exclude directories (causes error?)
                 # list.local.files <- list.local.files[use]

                  dontuse <- grep(".SIG", list.local.files, fixed = TRUE)
                  key <- list.local.files[-dontuse]
              }
		  ## see if the version of the repository has changed ## ## still needs work!

              	for (i in key){ 
               	   #if(!checkSIG(db, i)) # ignore .SIG files for now
			   # but, still need to make sure this getdata only gets new data when necessary
			   if(!checkRemote(db,i)) 
               	       getdata(db,i)
              	}	
		 
		} # end of if reposVersion = -1
		else print("no synchronization for a remoteDB object with a fixed version")
          })