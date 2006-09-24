## 'dbName' here should be a character vector with names 'url' and
## optionally 'dir', e.g. c(url = "http://....", dir = "/home/rpeng").

createRemote <- function(dbName) {
    nms <- names(dbName)

    if(is.null(nms))
        stop("'dbName' should have non-NULL names")
    if(!("url" %in% nms))
        stop("'dbName' should have a 'url' element")
    URL <- dbName["url"]

    ## Check to see if 'url' is of http:// form.  If so, get 'dir'
    ## element.  If 'url' is of file:// form, set the 'dir' element to
    ## be the directory to which file:// points.
    dir <- switch(checkURLtype(URL),
                  http = {
                      if(!("dir" %in% nms))
                          stop("'dbName' should have a 'dir' element")
                      else
                          dbName["dir"]
                  },
                  file = sub("^file://", "", URL, fixed = TRUE, perl = TRUE)
                  )    
    create(URL, dir)
    TRUE
}

## 'dbName' should be the name of a directory

initializeRemote <- function(dbName) {
    r <- init(dbName)

    switch(checkURLtype(r$url),
           http = {
               new("filehashRemote", url = r$url, dir = r$dir,
                   name = basename(dbName))
           },
           file = {
               new("filehashLocal", url = r$url, dir = r$dir,
                   name = basename(dbName))
           })
}

checkURLtype <- function(URL) {
    if(length(grep("^http://", URL, fixed = TRUE, perl = TRUE)) > 0)
        "http"
    else if(length(grep("^file://", URL, fixed = TRUE, perl = TRUE)) > 0)
        "file"
    else
        stop("'url' should be of type 'http://' or 'file://'")
}


setMethod("dbCreate",
          signature(db = "filehashRemote"),
          function(db, ...) {
		## remove trailing "/" on dir and url ##
		if (length(grep("/$",list(db@dir),perl = TRUE)) > 0)
       	     db@dir <- sub("/$","", db@dir)
		if (length(grep("/$",list(db@url),perl = TRUE)) > 0)
       	     db@url <- sub("/$","", db@url)
		## create the local main directory and data sub-directory to
		## store the data files ##
		dir.create(db@dir)
		dir.create(file.path(db@dir,"data"))
		## save url in the R workspace format in the main directory ##
		myurl <- db@url 
		save(myurl, file = file.path(db@dir,"url"))
          })

setMethod("dbCreate",
          signature(db = "filehashLocal"),
          function(db, ...) {
		## remove trailing "/" on dir and url ##
		if (length(grep("/$",list(db@dir),perl = TRUE)) > 0)
       	     db@dir <- sub("/$","", db@dir)
		## create the local main directory and data sub-directory to
		## store the data files ##
		dir.create(db@dir)
		dir.create(file.path(db@dir,"data"))
          })



####################
## 1) create #######
####################

create <- function(myurl,dir) {	
	## remove trailing "/" on dir and myurl ##
	if (length(grep("/$",list(dir),perl = TRUE)) > 0)
            dir <- sub("/$","", dir)
	if (length(grep("/$",list(myurl),perl = TRUE)) > 0)
            myurl <- sub("/$","", myurl)

	## create the local main directory and data sub-directory to
	## store the data files ##
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

