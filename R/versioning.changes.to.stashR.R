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

objectVersion <- function(db, key){ 
	info <- reposVersionInfo(db)
	if(length(info)!=0){
		keyFiles <- strsplit(info[length(info)], " : ")[[1]][2]
		keyFilesSep <- strsplit(keyFiles," ")[[1]]
		v <- grep(paste("^",key,"\\.[1-9]+$",sep=""),keyFilesSep,value=TRUE)
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
		as.numeric(strsplit(info[length(info)], " : ")[[1]][1])
	}
	else 0
}


## reads last line of the version file creates new info on repository
## version to be inserted in last line of the version file updates

updatedReposVersionInfo <- function(db, key){  ######### 
	reposV <- latestReposVersionNum(db)+1
	info <- reposVersionInfo(db)
	if(length(info)!=0){
		keyFiles <- strsplit(info[length(info)], " : ")[[1]][2]
		keyFilesSep <- strsplit(keyFiles," ")[[1]]
		v <- grep(paste("^",key,"\\.[1-9]+$",sep=""),keyFilesSep)

		if(length(v)==0)
                    others <- keyFilesSep
                else
                    others <- keyFilesSep[-v]
		updatedKeyFiles <- paste(paste(others, collapse=" "), 
                                         paste(key,objectVersion(db,key)+1,
                                               sep="."),
                                         sep=" ")
	}
	else updatedKeyFiles <- paste(key,1,sep=".")
	paste(reposV, updatedKeyFiles, sep = " : ") 
}


######### updating version file ########## 
updateVersion <- function(db,key){
	cat(updatedReposVersionInfo(db,key),
            file = file.path(dbLocal@dir,"version"),
            sep = "\n", append = TRUE)
}



###############################
## local.file.path ############  Creates a file path in the local data  
###############################  directory (to be used internally).	

local.file.path <- function(db,key, objVerNum){
    file.path(path.expand(db@dir), "data", paste(key,".",objVerNum,sep=""))
}

###############################
## local.file.path.SIG ######## Creates a file path in the local data  
############################### directory (to be used internally) for the SIG files.	

local.file.path.SIG <- function(db,key, objVerNum){
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
	vn <- objectVersion(db,key)
	
	if(!file.exists(datadir))
        stop("local data directory does not exist")
    	file.exists(local.file.path(db,key,vn))      ## returns a vector of T/F
}

read <- function(db, key){
    if(!checkLocal(db,key))
        stop(gettextf("files associated with key '%s' not yet downloaded", key))
    vn <- objectVersion(db,key)
    con <- gzfile(local.file.path(db,key,vn))
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
              con <- file(file.path(db@dir, "keys"))

              handler <- function(cond) {
                  character(0)
              }
              tryCatch({
                  open(con, "r")  ## 'keys' is a text file
                  readLines(con)
              }, error = handler, warning = handler, finally = {
                  if(isOpen(con))
                      close(con)
              })
          })

setMethod("dbExists", signature(db = "localDB", key = "character"),
          function(db, key, ...){
              key %in% dbList(db)	# returns a vector of T/F
          })

