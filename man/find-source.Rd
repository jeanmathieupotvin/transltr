% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find-source.R
\name{find_source}
\alias{find_source}
\alias{find_source_in_files}
\title{Find Source Text}
\usage{
find_source(
  path = getwd(),
  encoding = "UTF-8",
  strict = TRUE,
  id = uuid(),
  hash_algorithm = get_hash_algorithms(),
  native_languages = character(),
  verbose = FALSE
)

find_source_in_files(
  paths = character(),
  encoding = "UTF-8",
  strict = TRUE,
  hash_algorithm = get_hash_algorithms(),
  verbose = FALSE
)
}
\arguments{
\item{path}{A non-empty and non-\link[base:NA]{NA} character string. A path to
a directory containing \R source scripts. All subdirectories are searched.
Files that do not have a \code{.R}, or \code{.Rprofile} extension are skipped.}

\item{encoding}{A non-empty and non-\link[base:NA]{NA} character string. The
underlying source character encoding. In almost all cases, this should
be UTF-8. Other encodings are internally re-encoded to UTF-8 for
portability.}

\item{strict}{A non-\link[base:NA]{NA} logical value. Should only
\emph{explicit calls}, i.e. \code{transltr::translate()}, be extracted? If \code{FALSE},
calls to \emph{any} such function are extracted regardless of the underlying
namespace, i.e. \code{translate()}.}

\item{id}{A non-empty and non-\link[base:NA]{NA} character string. A globally
unique identifier for the \code{\link{Translator}} object. Beware of
plausible collisions when using user-defined values.}

\item{hash_algorithm}{A character string equal to one of the values returned
by \code{\link[=get_hash_algorithms]{get_hash_algorithms()}}. The algorithm to use when hashing source
information for identification purposes.}

\item{native_languages}{A named character vector of non-empty and
non-\link[base:NA]{NA} values. It can be empty. It is used to to construct
a mapping of language codes to native language names. See field
\code{\link[=Translator]{Translator$native_languages}} for more information.}

\item{verbose}{A non-\link[base:NA]{NA} logical value. Should basic information
on extracted source texts be outputted?}

\item{paths}{A character vector of non-empty and non-\link[base:NA]{NA} values.
A set of paths to \R source scripts that must be searched.}
}
\value{
\code{\link[=find_source]{find_source()}} returns an \code{\link[R6:R6Class]{R6}} object of class
\code{\link{Translator}}.

\code{\link[=find_source_in_files]{find_source_in_files()}} returns a list of \code{\link{Block}} objects. It may
contain duplicated elements, depending on the extracted contents.
}
\description{
Find, and extract source text that requires translation.
}
\details{
\code{\link[=find_source]{find_source()}} and \code{\link[=find_source_in_files]{find_source_in_files()}} look for calls to \code{\link[=translate]{translate()}}
in \R scripts, and convert them to \code{\link{Block}} objects via \code{\link[=as_block]{as_block()}}.

\code{\link[=find_source]{find_source()}} further constructs a \code{\link{Translator}} object from
the set of \code{\link{Block}} objects. It can later be exported, and imported
via \code{\link[=write_translations]{write_translations()}} and \code{\link[=read_translations]{read_translations()}} respectively.
\subsection{Methodology}{

Extracting source text from source code involves performing usual parsing
operations. \code{\link[=find_source]{find_source()}} and \code{\link[=find_source_in_files]{find_source_in_files()}} go through these
steps to extract source text from a single \R script.
\enumerate{
\item It is read with \code{\link[=read_text]{read_text()}}.
\item It is parsed with \code{\link[=parse]{parse()}}, and underlying tokens are extracted from
parsed expressions with \code{\link[utils:getParseData]{utils::getParseData()}}.
\item Each expression token (\code{expr}) is converted to language objects with
\code{\link[=str2lang]{str2lang()}}. Parsing errors, and invalid expressions are silently
skipped.
\item Valid \code{\link[=call]{call}} objects stemming from step 3 are filtered with
\code{\link[=is_translate_call]{is_translate_call()}}.
\item Calls to \code{\link[=translate]{translate()}} stemming from step 4 are coerced to
\code{\link{Block}} objects with \code{\link[=as_block]{as_block()}}.
}

\code{\link[=find_source]{find_source()}} further constructs a \code{\link{Translator}} object from
\code{\link{Block}} objects stemming from step 5.
}

\subsection{Limitations}{

The current version of \code{\link{transltr}} can only handle \strong{literal}
character vectors. This means it cannot process values passed to argument
\code{...} of \code{\link[=translate]{translate()}} that depends on any state at runtime. There are
plans to lift this limitation in the future.
}
}
\seealso{
\code{\link{Translator}},
\code{\link{Block}},
\code{\link[=translate]{translate()}},
\code{\link[=read_translations]{read_translations()}},
\code{\link[=write_translations]{write_translations()}}
}
