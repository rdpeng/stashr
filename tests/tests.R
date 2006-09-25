## Tests for Regression testing ##########################################
##########################################################################

library(filehashRemote)

##########################################################################
## Test objects of class 'filehashRemote'

myurl <- "http://www.biostat.jhsph.edu/MCAPS/data/"
wd <- getwd()
dir <- file.path(wd,"testDir")

## create a 'filehashRemote' object ##
db <- new("filehashRemote", url= myurl, dir = dir, name= "MCAPS")
show(db)
show(class(db))
show(db@url)
show(db@dir)


## other prelim steps necessary ##
dbCreate(db)

## test the methods ##
dbList(db)
dbList(db, save = TRUE)
dbFetch(db, "01003") 
dbFetch(db, "01003", offline = TRUE) 
try( dbFetch(db, "01004") )
try( dbFetch(db, "01004", offline = TRUE) )
try( dbDelete(db,"01003") )
try( dbInsert(db,key = "01004", value = NULL) )
dbSync(db, key = NULL)
dbSync(db, key = c("01003"))
try( dbSync(db, key = c("01004","01003")) )
dbExists(db,c("01003", "01004","55079"))

## remove db@dir directory ##
unlink(db@dir, recursive = TRUE)

##########################################################################
## Test objects of class 'filehashLocal'

## create a 'filehashRemote' object ##
dbLocal <- new("filehashLocal", dir= dir, name= "MCAPS")
show(dbLocal)
show(class(dbLocal))
show(dbLocal@dir)

dbCreate(dbLocal)

## test the methods  ##
dbInsert(dbLocal,key = "01004", value = 1:10, overwrite = FALSE)
try( dbInsert(dbLocal,key = "01004", value = 1:10, overwrite = FALSE) )
dbList(dbLocal)
dbInsert(dbLocal,key = "01005", value = rep(5,10), overwrite = FALSE)
dbInsert(dbLocal,key = "01006", value = matrix(1,5,4), overwrite = FALSE)
dbList(dbLocal)
dbFetch(dbLocal, "01004") 
try( dbFetch(dbLocal, "01003") )
dbFetch(dbLocal, "01005")  # should work, doesn't right now!
dbDelete(dbLocal,"01004")
try( dbDelete(dbLocal,"01004") )
dbDelete(dbLocal,"01005")	#doesn't delete both files? (b/c file is empty)
dbList(dbLocal)
dbExists(dbLocal,key="01004")
dbExists(dbLocal,key="01006")


