\name{utils}
\alias{setDir<-}
\alias{setDir<-,remoteDB,character-method}

\title{Set directory for remote database}

\description{
  Set the directory for the local cache of a \code{"remoteDB"} database.
}

\usage{
setDir(db) <- value
}

\arguments{
  \item{db}{an object of class \code{"remoteDB"}}
  \item{value}{character, directory to be assigned}
}

\details{
  Users obtaining \code{"remoteDB"} objects created on different
  machines may want to reset the directory specified for the local
  cache.  This function is to be used for that purpose.
}

\keyword{utilities}
