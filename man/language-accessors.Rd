% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/language.R
\name{language_set}
\alias{language_set}
\alias{language_get}
\title{Get or Set Language}
\usage{
language_set(lang = "en")

language_get()
}
\arguments{
\item{lang}{A non-empty and non-\link[base:NA]{NA} character string. The
requested language.

A language is usually a code (of two or three letters) for a native
language name. For example, \code{ja} is a code for \code{日本語} (Japanese). While
users retain full control over codes, it is best to use language codes
stemming from well-known schemes such as
\href{https://en.wikipedia.org/wiki/IETF_language_tag}{IETF BCP 47}, or
\href{https://en.wikipedia.org/wiki/List_of_ISO_639_language_codes}{ISO 639-1}.
Doing so maximizes portability and cross-compatibility between packages.}
}
\value{
\code{\link[=language_set]{language_set()}} returns \code{NULL} invisibly. It is used for its side-effect
of setting environment variable \code{TRANSLTR_LANGUAGE}.

\code{\link[=language_get]{language_get()}} returns a character string (possibly empty). It corresponds
to the value of environment variable \code{TRANSLTR_LANGUAGE}.
}
\description{
Get or set the current language of \code{\link{transltr}}. It is registered
as an environment variable named \code{TRANSLTR_LANGUAGE}.
}
\details{
\code{\link[=language_set]{language_set()}} leaves the underlying locale as is. To change an \R
session's locale, use \code{\link[=Sys.setlocale]{Sys.setlocale()}} or \code{\link[=Sys.setLanguage]{Sys.setLanguage()}} instead.
See below for more information.
}
\note{
An environment variable is used because it can be shared among different
processes. This matters when using parallel and/or concurrent \R sessions.
It can further be shared among direct and transitive dependencies (other
packages that rely on \code{\link{transltr}}).
}
\section{Locales versus languages}{

A \href{https://en.wikipedia.org/wiki/Locale_(computer_software)}{locale} is a
set of multiple low-level settings that relate to the user's language and
region. The \emph{language} itself is just one parameter among many others.

Modifying a locale on-the-fly \emph{can} be considered risky in some situations.
It may not be the optimal solution for merely changing textual representations
of a program or an application at runtime, as it may introduce unintended
changes and induce subtle bugs that are harder to fix.

Moreover, it makes sense for some applications and/or programs such as
\href{https://shiny.posit.co/}{Shiny applications} to decouple the front-end's
current language (what \emph{users} see) from the back-end's locale (what
\emph{developers} see). A UI may be displayed in a certain language while keeping
logs and \R internal \link[=message]{messages}, \link[=warning]{warnings}, and
\link[=stop]{errors} as is (untranslated).

Consequently, the language setting of \code{\link{transltr}} is purposely
kept separate from the underlying locale and removes the complexity of
having to support many of them. Users can always change both the locale and
the \code{language} parameter of the package. See Examples.
}

\examples{
## Change the language parameter (globally).
language_set("fr")
language_get()  ## Outputs "fr".

## Change both the language parameter and the locale.
## Note that you control how languages are named for language_set(),
## but not for Sys.setLanguage().
language_set("fr")
Sys.setLanguage("fr-CA")

## Reset settings.
language_set(NULL)

}
