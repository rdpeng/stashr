####################
## 1) create #######
####################

create <- function(myurl,dir){	
	dir.create(dir)
	save(myurl, file = paste(dir,"/url.R",sep=""))
}

####################
## 2) init #########
####################

init <- function(dir){
	load(paste(dir,"/url.R",sep=""))
	list(myurl,dir)
	}

####################
## 3) fetch ########
####################

fetch <- function(db,key){
	con <- gzcon(url(paste(db[[1]],key,sep="")))
	open(con, "rb")
	dat <- unserialize(con)
	close(con)
	save(dat, file = paste(db[[2]],"/",key,".R",sep=""))
}

####################
## 4) read  ########
####################

read <- function(db,key){
	load(paste(db[[2]],"/",key,".R",sep=""))
	dat
	}

####################
## 5) insert #######
####################

#note that value is an R object#

insert <- function(db,key,value){
	if(file.exists(paste(db[[2]],"/",key,".R",sep="")))
		stop("cannot overwrite previously saved file")
	else	{save(value, file = paste(db[[2]],"/",key,".R",sep=""))}
	}

############################################
## Testing the functions ###################
############################################

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
insert(db.test,key.test,value=dat)

## when a new file is to be saved ##	
key.test2 <- "01027"
	# get the key.test2 data #
	con <- gzcon(url(paste(db.test[[1]],key.test2,sep="")))
	open(con, "rb")
	dat <- unserialize(con)
	close(con)
insert(db.test,key.test2,value=dat)

	




