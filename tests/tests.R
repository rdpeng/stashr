## Tests for Regression testing ##########################################
##########################################################################

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
create(db@url,db@dir)

## test the methods ##
dbList(db)
dbList(db, save = TRUE)
dbFetch(db, "01003") 
dbFetch(db, "01003", offline = TRUE) 
dbFetch(db, "01004")
dbFetch(db, "01004", offline = TRUE)
dbDelete(db,"01003")
dbInsert(db,key = "01004", value = NULL)
dbSync(db, key = NULL)
dbSync(db, key = c("01003"))
dbSync(db, key = c("01004","01003"))
dbExists(db,c("01003", "01004","55079"))
