## Prelim Functions  ###################################################
########################################################################

###############################
## local.file.path ############  Creates a file path in the local data  
###############################  directory (to be used internally).	

local.file.path <- function(db,key){
	file.path(db[["dir"]],"data",key)
}

###############################
## local.file.path.SIG ######## Creates a file path in the local data  
############################### directory (to be used internally) for the SIG files.	

local.file.path.SIG <- function(db,key){
	file.path(db[["dir"]],"data",paste(key,".SIG",sep=""))
}

#######################
## getlist ############ Reads the 'keys' file from the server. Has an option 
####################### to save the repository's list of keys in local dir.

getlist <- function(db, save=FALSE){
	con <- gzcon(url(file.path(db[["url"]],"keys.gz")))
	open(con, "rb")
	on.exit(close(con))
	mylist <- readLines(con)
	if (save) save(mylist, file = file.path(db[["dir"]],"list"))
	mylist 
	}

#################### Returns TRUE if data file for 'key' is in local dir, otherwise
## checkLocal ###### returns FALSE. We have 'key' allowed to be a character vector
#################### with more than one key.

checkLocal <- function(db, key){
	key %in% getlist(db, save = FALSE)	# returns a vector of T/F
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
	download.file(file.path(db[["url"]],"db",key), local.file.path(db,key), mode="wb", cacheOK=FALSE)
	download.file(file.path(db[["url"]],"db",paste(key,".SIG",sep="")), local.file.path.SIG(db,key), mode="wb", cacheOK=FALSE)
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
		{list.local.files <- list.files(file.path(db[["dir"]],"data"))
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

## Testing the functions ############################################################
#####################################################################################

#### testing the create function ####
myurl.test <- "http://www.biostat.jhsph.edu/MCAPS/data/"
dir.test <- "C:/Documents and Settings/Sandrah Eckel/My Documents/School/Summer 2006/Project with Roger/Test Dir"
create(myurl.test,dir.test)

#### testing the init function ####
db.test <- init(dir.test)

#### testing the getdata function ####
key.test <- "01003"
getdata(db.test,key.test)
#getdata(db.test,"01027")

#### testing the read function ####
mydat <- read(db.test,key.test)
read(db.test,"01004") # should give error

#### testing the fetch function ####
fetch.dat <- fetch(db.test,key.test)
fetch.dat <- fetch(db.test,key.test,offline=TRUE)

#### testing the update function ####
key.junk <- c("01003","01027")
key.junk2 <- c("01003","01004")
update(db.test,key=key.junk)
update(db.test,key=key.junk2)
update(db.test)
update(db.test,key="01003")

#### testing the insert function ####
	## when the file is already saved ##
	insert(db.test,key.test,value=dat,overwrite=FALSE)

	## when the file is already saved, overwrite=TRUE ##
	insert(db.test,key.test,value=dat,overwrite=TRUE)

	## when a new file is to be saved ##	
	key.test2 <- "01027"
		# get the key.test2 data #
		con <- gzcon(url(file.path(db.test[["url"]],"db",key.test2)))
		open(con, "rb")
		dat <- unserialize(con)
		close(con)
	insert(db.test,key.test2,value=dat)


#### testing local.file.path ####
local.file.path(db.test,key.test)

#### testing local.file.path.SIG ####
local.file.path.SIG(db.test,key.test)

#### testing getlist ####
getlist(db.test, save=FALSE)
getlist(db.test, save=TRUE)

#### testing checkLocal ####
checkLocal(db.test,key.junk2)
checkLocal(db.test,key.junk)
checkLocal(db.test,key.test)
checkLocal(db.test, "01004")




