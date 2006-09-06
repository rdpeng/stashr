## Tests for Regression testing ##########################################
##########################################################################

library(filehashRemote)

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
