% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transltr-params.R
\name{get_template_versions}
\alias{get_template_versions}
\alias{get_hash_algorithms}
\alias{get_hash_length_range}
\alias{get_generated_by}
\alias{get_generated_on}
\title{Parameters}
\usage{
get_template_versions()

get_hash_algorithms()

get_hash_length_range(hash_algorithm = get_hash_algorithms())

get_generated_by()

get_generated_on()
}
\arguments{
\item{hash_algorithm}{A character string. The algorithm to use when
hashing and identifying source texts that requires translation. It
must be a value returned by \code{\link[=get_hash_algorithms]{get_hash_algorithms()}}.}
}
\value{
\code{\link[=get_template_versions]{get_template_versions()}} returns an integer vector.

\code{\link[=get_hash_algorithms]{get_hash_algorithms()}} returns a character vector.

\code{\link[=get_hash_length_range]{get_hash_length_range()}} returns a named integer vector of length 2 which
represents the range of valid values for the hashes' length (\strong{in bytes}).

\code{\link[=get_generated_by]{get_generated_by()}} returns a character string.

\code{\link[=get_generated_on]{get_generated_on()}} is just a semantic alias for \code{\link[=utc]{utc()}} and returns
whatever it returns.
}
\description{
Helper functions that return valid values for various parameters and
components of package \code{\link{transltr}}.
}
\details{
From the users' perspective, the hash length correspond to the number of
hexadecimal characters for convenience. Internally, it corresponds to a
number of bytes.
}
\seealso{
\code{\link[=utc]{utc()}}
}
\concept{internal parameters}
\keyword{internal}
