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
