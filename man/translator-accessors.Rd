% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/translator.R
\name{translator_set}
\alias{translator_set}
\alias{translator_get}
\alias{translator_scopes}
\title{Get or Set Translator Objects}
\usage{
translator_set(x = translator(), scope = NULL)

translator_get(scope = NULL)

translator_scopes()
}
\arguments{
\item{x}{A \code{\link{Translator}} object, or \code{NULL}. The latter is
used to unset a previously registered \code{\link{Translator}} object.}

\item{scope}{A \code{NULL}, or an optional non-empty and non-\link[base:NA]{NA}
character string.}
}
\value{
\code{\link[=translator_set]{translator_set()}} returns \code{NULL}, invisibly. It is used for its side-effect
of registering \code{x}, and assigning it to a \code{scope}.

\code{\link[=translator_get]{translator_get()}} returns the \code{\link{Translator}} object assigned
to \code{scope}, or \code{NULL} if there is none.

\code{\link[=translator_scopes]{translator_scopes()}} returns a character vector.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Get or set the \code{\link{Translator}} object to use when calling
\code{\link[=translate]{translate()}}. It is registered within an internal cache managed by
\code{\link{transltr}}.
}
\details{
Translations performed by \code{\link[=translate]{translate()}} always have a \code{scope} specifying
which \code{\link{Translator}} object must be used. This scope can be
explicit, or implicit.
\subsection{Explicit Scopes}{

Explicit scopes are entirely determined, and managed by the user. In that
case, argument \code{scope} of \code{\link[=translator_set]{translator_set()}}, \code{\link[=translator_get]{translator_get()}}, and
\code{\link[=translate]{translate()}} must be set to the appropriate value. \strong{Scopes are shared},
and \strong{can be overwritten}. When choosing to use explicit scopes, it is the
responsability of the user to ensure its scope(s) are collision-resistant.
It may use \code{\link[=translator_scopes]{translator_scopes()}} to get a list of active scopes.
}

\subsection{Implicit Scopes}{

Implicit scopes are the default. These are scopes entirely determined, and
managed by \code{\link{transltr}} (unless a user \emph{unsets} them). They are
inferred from the underlying \link[=getNamespace]{namespace} of the function
that (directly) calls either \code{\link[=translator_set]{translator_set()}}, \code{\link[=translator_get]{translator_get()}}, or
\code{\link[=translate]{translate()}}. If it has no namespace, the \code{global} scope is used. There is
one exception: package \code{base} is always ignored and replaced by the global
scope. In other words, the scope will be \code{global}, unless \code{\link[=translator_set]{translator_set()}},
\code{\link[=translator_get]{translator_get()}}, or \code{\link[=translate]{translate()}} is directly called within a function
defined in any non-\code{base} package.

Implicit scopes are determined at runtime by inspecting the call stack. \R
call stacks can be surprisingly complex, and in some non-standard situations,
\code{\link{transltr}} may fail to infer a scope. Future iterations will
bring more robust scoping mechanisms.
}
}
\section{Reference semantics}{

\code{\link{Translator}} objects are \code{\link[R6:R6Class]{R6}} objects, and
\code{\link[R6:R6Class]{R6}} are stored as \link[=environment]{environments}. In \R,
environments have reference semantics (they are not copied when changed).
Therefore, a \code{\link{Translator}} object only need to be set once,
and may be modified afterwards without having to call \code{\link[=translator_set]{translator_set()}}
again.

Moreover, removing a binding that references a \code{\link{Translator}}
object does not remove it from the internal cache. Users further have to
call \code{\link[=translator_set]{translator_set()}} to do so.
}

\examples{
## Setting and getting a Translator object having a 'global' scope.
translator_set(x = translator(id = "my-global-translator"))
translator_get()
translator_get("global")

## Unregister a Translator object (for implicit and explicit scopes).
translator_set(NULL)
translator_set(NULL, "global")
is.null(translator_get(NULL)) ## TRUE

## Setting and getting a Translator object, while letting them infer
## the underlying scope. This is done temporarily within the utils
## package for illustration purposes.
evalq(envir = asNamespace("utils"), \() {
   on.exit(translator_set(NULL))
   translator_set(translator(id = sprintf("utils:\%s", transltr::uuid())))
   return(translator_get())
})()

}
\seealso{
\code{\link[=translate]{translate()}}
}
