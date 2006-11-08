\name{stashROption}
\alias{stashROption}

\title{Set options}
\description{
  Set options relevant to the \code{stashR} package
}
\usage{
stashROption(name, value)
}

\arguments{
  \item{name}{character, name of option}
  \item{value}{value for the option}
}
\details{
  Currently, the only option that can be set is \code{quietDownload}
  which is a logical indicating whether download progress should be
  printed to the screen when downloading a value associated with a
  specific key.  The default is \code{TRUE}.
}
\author{Roger D. Peng}
\keyword{utilities}
