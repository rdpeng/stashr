## Prelim Functions  ###################################################
########################################################################

###############################
## local.file.path ############  creates a file path in the local data  
###############################  directory (to be used internally)	

local.file.path <- function(db,key){
	file.path(db[["dir"]],"data",key)
}

#######################
## getlist ############  reads the 'keys' file from the server 
#######################

## wasn't sure if we wanted this to just return the list 
## or to also save a 'list' file, so I included an option

getlist <- function(db, save=FALSE){
	con <- gzcon(url(file.path(db[["url"]],"keys.gz")))
	open(con, "rb")
	on.exit(close(con))
	mylist <- readLines(con)
	if (save) save(mylist, file = file.path(db[["dir"]],"list"))
	mylist 
	}

#######################
## lookup #############  checks to see if a given key is in the local database 
#######################

lookup <- function(db,key){
	key %in% getlist(db.test, save=FALSE)
	}


## Five Functions ##########################################################
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
## 3) fetch ########
####################

fetch <- function(db,key){
	con <- gzcon(url(file.path(db[["url"]],"db",key)))
	open(con, "rb")
	on.exit(close(con))
	dat <- unserialize(con)
	save(dat, file = local.file.path(db,key))
}

####################
## 4) read  ########
####################

read <- function(db,key){
	load(local.file.path(db,key))
	dat
	}

####################
## 5) insert #######
####################

#note that value is an R object#

insert <- function(db,key,value,overwrite=FALSE){
	if(file.exists(local.file.path(db,key)) & !overwrite)
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

#### testing the fetch function ####
key.test <- "01003"
fetch(db.test,key.test)

#### testing the read function ####
mydat <- read(db.test,key.test)

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

#### testing getlist ####
getlist(db.test, save=FALSE)

#### testing lookup ####
lookup(db.test, "01004")






