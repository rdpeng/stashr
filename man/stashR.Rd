\name{stashR}
\docType{class}
\alias{remoteDB}
\alias{remoteDB-class}
\alias{localDB-class}
\alias{dbUnlink,localDB-method}
\alias{dbUnlink}
\alias{dbInsert,localDB,character-method}
\alias{dbFetch,localDB,character-method}
\alias{dbDelete,localDB,character-method}
\alias{dbList,localDB-method}
\alias{dbExists,localDB,character-method}
\alias{dbDelete,remoteDB,character-method}
\alias{dbExists,remoteDB,character-method}
\alias{dbFetch,remoteDB,character-method}
\alias{dbInsert,remoteDB,character-method}
\alias{dbList,remoteDB-method}
\alias{dbList}
\alias{dbSync,remoteDB-method}
\alias{dbSync}
\alias{dbCreate,remoteDB-method}
\alias{dbCreate,localDB-method}
\alias{dbCreate}


\title{Classes "remoteDB" and "localDB"}
\description{
	These functions create an interface for a simple file-based 
	key-value database using remote storage via http
}

\section{Objects from the "remoteDB" and "localDB" classes}{
	Objects can be created by calls of the form 
	\code{new("remoteDB", ...)} and \code{new("localDB", ...)}
	respectively.
}

\section{Slots}{
	 \describe{
		\item{\code{url}:}{Object of class \code{"character"}, 
			url of the remote database}
		\item{\code{dir}:}{Object of class \code{"character"}, 
			local directory in which to download the data or local 
			directory in which to create a database}
  }
}

\section{Methods}{
  \describe{
    \item{dbCreate}{\code{signature(db = "remoteDB")}: 
	Calling \code{dbCreate} on a \code{remoteDB} object creates the 
	local main directory and data sub-directory to in which to store the 
	data files and saves the url associated with the \code{remoteDB}
	object in the R workspace format in the local main directory.}
    \item{dbCreate}{\code{signature(db = "localDB")}: 
	Calling \code{dbCreate} on a \code{localDB} object creates the 
	local main directory and data sub-directory to in which to store the 
	data files.}
    \item{dbDelete}{\code{signature(db = "remoteDB", key = "character")}: 
	Calling \code{dbDelete} on a \code{remoteDB} object returns the 
	error message: "cannot delete from a 'remoteDB' database."}
    \item{dbDelete}{\code{signature(db = "localDB", key = "character")}: 
	Calling \code{dbDelete} on a \code{localDB} deletes the specified
	file and its corresponding .SIG file from your local directory. It also
	removes the specified key from the \code{keys} file.}
    \item{dbExists}{\code{signature(db = "remoteDB", key = "character")}: 
	The \code{key} argument is a character vector. For each element in 
	\code{key}, \code{dbExists} returns TRUE if the corresponding data file 
	is in the local directory and otherwise returns FALSE.} 
    \item{dbExists}{\code{signature(db = "localDB", key = "character")}: 
	The \code{key} argument is a character vector. For each element in 
	\code{key}, \code{dbExists} returns TRUE if the corresponding data file 
	is in the local directory and otherwise returns FALSE.} 
    \item{dbFetch}{\code{signature(db = "remoteDB", key = "character")}: 
	Checks if the provided character value \code{key} exists in the local 
	directory. If it doesn't, \code{dbFetch} downloads the corresponding data 
	and SIG files. If the \code{key} exists in the local directory, then 
	\code{dbFetch} compares the SIG file in the local directory to the SIG 
	file in the repository. If the SIG files are the same, \code{dbFetch} 
	reads the file from the local directory and otherwise re-downloads the 
	data and SIG files. If \code{offline = TRUE}, \code{dbFetch} skips the 
	download step. The function returns the data object associated with the 
	\code{key}. }
    \item{dbFetch}{\code{signature(db = "localDB", key = "character")}: 
	Checks if the provided character value \code{key} exists in the local 
	directory. If the \code{key} exists in the local directory, then 
	\code{dbFetch} reads the file from the local directory. The function 
	returns the data object associated with the \code{key}. }
    \item{dbInsert}{\code{signature(db = "remoteDB", key = "character",
	value = "ANY")}: 
	Calling \code{dbInsert} on a \code{remoteDB} object returns the 
	error message: "cannot insert into a 'remoteDB' database." }
    \item{dbInsert}{\code{signature(db = "localDB", key = "character", 
	value = "ANY")}: 
	Calling \code{dbInsert} on a \code{localDB} object writes the 
	value to a file corresponding to the specified key in the local 
	directory. Also, \code{dbInsert} appends the specified key to the end
	of the \code{keys} file if the key is not already included in the 
	\code{keys} file. }
    \item{dbList}{\code{signature(db = "remoteDB")}: 
	The method \code{dbList} reads the \code{keys} file from the remote
	database and has the option \code{save} to save the repository's list 
	of keys in the local directory.}
    \item{dbList}{\code{signature(db = "localDB")}: 
	The method \code{dbList} called on a \code{localDB} object reads 
	the \code{keys} file from the local directory and returns the vector of keys.}
    \item{dbSync}{\code{signature(db = "remoteDB", key = "character")}: 
	 Updates all key/data pairs in the local directory by checking the
	SIGs if \code{key = NULL}.  If \code{key} is a character vector, then it
	only updates the specified key/data pairs (in which case, it first
	checks to ensure that all specified keys' files have been previously saved).}
	 }
}

\author{Sandy Eckel, Roger D. Peng}

\examples{
\dontrun{
## Objects of the class localDB

wd <- getwd()
dir <- file.path(wd,"localDBExample")

## Create local stashR data repository 'localDBExample'
fhLocal <- new("localDB", dir = dir, name = "localDBExample")

## Insert key-value data into 'localDBExample' 
v <- 1:10
dbInsert(fhLocal,key = "vector", value = v, overwrite = TRUE)
m <- matrix(1:20,5,4)
dbInsert(fhLocal,key = "matrix", value = m, overwrite = TRUE)
d <- data.frame(cbind(id = 1:5, age=c(12,11,15,11,14), sex = c(1,1,0,1,0)))
dbInsert(fhLocal,key = "dataframe", value = d, overwrite = TRUE)
l <- list(v = v, m = m, df = d)
dbInsert(fhLocal, key = "list", value = l, overwrite = TRUE)

dbList(fhLocal)

dbFetch(fhLocal, "dataframe") 
dbDelete(fhLocal, "vector")
dbExists(fhLocal, "vector")
dbList(fhLocal) 


## Objects of the class remoteDB

## The same key-value data used in the previous example for localDB
## has been stored in a remoteDB repository on the internet at:
myurl <- "http://www.biostat.jhsph.edu/~seckel/remoteDBExample"

wd <- getwd()
dir <- file.path(wd,"remoteDBExample") 
## Create local copy of data repository 'remoteDBExample'
fhRemote <- new("remoteDB", url= myurl, 
            dir = dir, name= "remoteDBExample")

dbList(fhRemote, save = TRUE)
dbExists(fhRemote,c("vector", "array","list", "function"))
## downloads 'vector' data from the remoteDB repository
dbFetch(fhRemote, "vector") 
## fetches without downloading again 
dbFetch(fhRemote, "vector") 
## synchronize all local copies of the data to the remote version
dbSync(fhRemote, key = NULL)
}
}
\keyword{classes}
