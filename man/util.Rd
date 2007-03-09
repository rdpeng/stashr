\name{utils}

\alias{setDir<-}
\alias{setDir<-,remoteDB,character-method}

\alias{initialize,remoteDB-method}
\alias{initialize,localDB-method}

\alias{copyDB}
\alias{copyDB,localDB-method}

\alias{currentReposVersion}
\alias{currentReposVersion,localDB-method}
\alias{currentReposVersion,remoteDB-method}

\alias{currentReposVersion<-}
\alias{currentReposVersion<-,localDB,numeric-method}
\alias{currentReposVersion<-,remoteDB,numeric-method}

\title{Set directory for remote database}

\description{
  Utilities for \code{"localDB"} and \code{"remoteDB"} databases
}

\usage{
setDir(db) <- value
copyDB(db, ...)
currentReposVersion(db, ...)
currentReposVersion(db) <- value

\S4method{initialize}{localDB}(.Object, ...)
\S4method{initialize}{remoteDB}(.Object, ...)

\S4method{copyDB}{localDB}(db, dir, ...)
}

\arguments{
  \item{db}{an object of class \code{"remoteDB"} or \code{"localDB"}}
  \item{value}{character, directory to be assigned}
  \item{.Object}{an object of class \code{"localDB"} or
    \code{"remoteDB"}}
  \item{dir}{directory into which the \code{"localDB"} should be copied}
  \item{...}{other arguments passed to methods}
}

\details{
  Users obtaining \code{"remoteDB"} objects created on different
  machines may want to reset the directory specified for the local
  cache.  This function is to be used for that purpose.

  Users generally will not call the \code{initialize} functions
  directly, but rather will call them indirectly via the \code{new}
  function.

  \code{copyDB} is used to copy a \code{"localDB"} database to another
  location (i.e. directory).

  \code{currentReposVersion} returns the current repository version
  number.
}

\keyword{utilities}
