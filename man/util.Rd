\name{utils}
\alias{setDir<-}
\alias{setDir<-,remoteDB,character-method}
\alias{initialize,remoteDB-method}
\alias{initialize,localDB-method}

\title{Set directory for remote database}

\description{
  Utilities for \code{"localDB"} and \code{"remoteDB"} databases
}

\usage{
setDir(db) <- value

\S4method{initialize}{localDB}(.Object, ...)
\S4method{initialize}{remoteDB}(.Object, ...)
}

\arguments{
  \item{db}{an object of class \code{"remoteDB"}}
  \item{value}{character, directory to be assigned}
  \item{.Object}{an object of class \code{"localDB"} or
    \code{"remoteDB"}}
  \item{...}{other arguments passed to methods}
}

\details{
  Users obtaining \code{"remoteDB"} objects created on different
  machines may want to reset the directory specified for the local
  cache.  This function is to be used for that purpose.

  Users generally will not call the \code{initialize} functions
  directly, but rather will call them indirectly via the \code{new}
  function.
}

\keyword{utilities}
