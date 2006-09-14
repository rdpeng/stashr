\name{filehashRemote}
\docType{class}
\alias{filehashRemote}
\alias{filehashRemote-class}
\alias{filehashLocal-class}
\alias{dbInsert,filehashLocal,character-method}
\alias{dbFetch,filehashLocal,character-method}
\alias{dbDelete,filehashLocal,character-method}
\alias{dbDelete,filehashRemote,character-method}
\alias{dbExists,filehashRemote,character-method}
\alias{dbFetch,filehashRemote,character-method}
\alias{dbInsert,filehashRemote,character-method}
\alias{dbList,filehashRemote-method}
\alias{dbSync,filehashRemote-method}
\alias{dbSync}
\alias{dbCreate,filehashRemote-method}
\alias{dbCreate}

\title{Classes "filehashRemote" and "filehashLocal"}
\description{
	These functions create an interface for a simple file-based 
	key-value database using remote storage via http
}

\section{Objects from the "filehashRemote" and "filehashLocal" classes}{
	Objects can be created by calls of the form 
	\code{new("filehashRemote", ...)} and \code{new("filehashLocal", ...)}
	respectively.
}

\section{Slots}{
	 \describe{
		\item{\code{url}:}{Object of class \code{"character"}, 
			url of the remote database}
		\item{\code{dir}:}{Object of class \code{"character"}, 
			local directory in which to download the data}
  }
}

\section{Methods}{
  \describe{
    \item{dbCreate}{\code{signature(db = "filehashRemote")}: 
	Calling \code{dbCreate} on a \code{filehashRemote} object creates the 
	local main directory and data sub-directory to in which to store the 
	data files and saves the url associated with the \code{filehashRemote}
	object in the R workspace format in the local main directory.}
    \item{dbDelete}{\code{signature(db = "filehashRemote", key = "character")}: 
	Calling \code{dbDelete} on a \code{filehashRemote} object returns the 
	error message: "cannot delete from a 'filehashRemote' database."}
    \item{dbDelete}{\code{signature(db = "filehashLocal", key = "character")}: 
	Calling \code{dbDelete} on a \code{filehashLocal} deletes the specified
	file and its corresponding .SIG file from your local directory.}
    \item{dbExists}{\code{signature(db = "filehashRemote", key = "character")}: 
	The \code{key} argument is a character vector. For each element in 
	\code{key}, \code{dbExists} returns TRUE if the corresponding data file 
	is in local dir and otherwise returns FALSE.} 
    \item{dbFetch}{\code{signature(db = "filehashRemote", key = "character")}: 
	Checks if the provided character value \code{key} exists in the local 
	directory. If it doesn't, \code{dbFetch} downloads the corresponding data 
	and SIG files. If the \code{key} exists in the local directory, then 
	\code{dbFetch} compares the SIG file in the local directory to the SIG 
	file in the repository. If the SIG files are the same, \code{dbFetch} 
	reads the file from the local directory and otherwise re-downloads the 
	data and SIG files. If \code{offline = TRUE}, \code{dbFetch} skips the 
	download step. The function returns the data object associated with the 
	\code{key}. }
    \item{dbFetch}{\code{signature(db = "filehashLocal", key = "character")}: 
	Checks if the provided character value \code{key} exists in the local 
	directory. If the \code{key} exists in the local directory, then 
	\code{dbFetch} reads the file from the local directory. The function 
	returns the data object associated with the \code{key}. }
    \item{dbInsert}{\code{signature(db = "filehashRemote", key = "character",
	value = "ANY")}: 
	Calling \code{dbInsert} on a \code{filehashRemote} object returns the 
	error message: "cannot insert into a 'filehashRemote' database." }
    \item{dbInsert}{\code{signature(db = "filehashLocal", key = "character", 
	value = "ANY")}: 
	Calling \code{dbInsert} on a \code{filehashLocal} object writes the 
	value to a file corresponding to the specified key in the local 
	directory. }
    \item{dbList}{\code{signature(db = "filehashRemote")}: 
	The method \code{dbList} reads the \code{keys} file from the remote
	database and has the option \code{save} to save the repository's list 
	of keys in the local directory.}
    \item{dbSync}{\code{signature(db = "filehashRemote", key = "character")}: 
	 Updates all key/data pairs in the local directory by checking the
	SIGs if \code{key = NULL}.  If \code{key} is a character vector, then it
	only updates the specified key/data pairs (in which case, it first
	checks to ensure that all specified keys' files have been previously saved).}
	 }
}

\author{Sandy Eckel, Roger D. Peng}

\seealso{
	\code{\link{filehash-class}}
}
\examples{
## ???
}
\keyword{classes}
