## Tests for Regression testing ##########################################
##########################################################################

library(stashR)

##########################################################################
## Test objects of class 'remoteDB'
myurl <- "http://www.biostat.jhsph.edu/MCAPS/data/"
wd <- getwd()
dir <- file.path(wd,"testDir")

## create a 'remoteDB' object ##
db <- new("remoteDB", url= myurl, dir = dir, name= "MCAPS")
show(db)
show(class(db))
show(db@url)
show(db@dir)


## other prelim steps necessary ##
dbCreate(db)

## test the methods ##
dbList(db)
dbList(db, save = TRUE)
dbFetch(db, "01073") 
dbFetch(db, "01073", offline = TRUE) 
try( dbFetch(db, "01004") )
try( dbFetch(db, "01004", offline = TRUE) )
try( dbDelete(db,"01073") )
try( dbInsert(db,key = "01004", value = NULL) )
dbSync(db, key = NULL)
dbSync(db, key = c("01073"))
try( dbSync(db, key = c("01004","01073")) )
dbExists(db,c("01073", "01004","55079"))

## remove db@dir directory ##
unlink(db@dir, recursive = TRUE)

##########################################################################
## Test objects of class 'localDB'

## create a 'remoteDB' object ##
dbLocal <- new("localDB", dir= dir, name= "MCAPS")
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
try( dbFetch(dbLocal, "01073") )
dbFetch(dbLocal, "01005")
dbDelete(dbLocal,"01004")
dbList(dbLocal)	
try( dbDelete(dbLocal,"01004") )
dbDelete(dbLocal,"01005")
dbList(dbLocal)
dbExists(dbLocal,key="01004")
dbExists(dbLocal,key="01006")


