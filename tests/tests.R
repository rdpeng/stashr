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


##########################################################################
## Test objects of class 'filehashLocal'

## (create a larger local database using 'filehashRemote') ##
dbFetch(db, "55079")
dbFetch(db, "55087")
dbFetch(db, "55089")
dbFetch(db, "55105")

## create a 'filehashRemote' object ##
dirLocal <- db@dir
dbLocal <- new("filehashLocal", dir= dirLocal, name= "MCAPS")
show(dbLocal)
show(class(dbLocal))
show(dbLocal@dir)

## test the methods  ##
dbFetch(dbLocal, "01003") 
try( dbFetch(dbLocal, "01004") )
dbDelete(dbLocal,"01003")
try( dbDelete(dbLocal,"01005") )
dbInsert(dbLocal,key = "01004", value = 1:10) 
dbFetch(dbLocal, "01004")
try( dbInsert(dbLocal,key = "01004", value = 1:10, overwrite=FALSE) )