#' Support Many Languages in R Programs
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' An object model for source text and translations. Find and extract
#' translatable strings. Provide translations and seamlessly retrieve
#' them at runtime.
#'
#' @section Introduction:
#' \R relies on GNU [`gettext`](https://www.gnu.org/software/gettext/) to
#' produce multi-lingual messages (if *Native Language Support* is enabled).
#' This is well-designed software offering an extensive set of functionalities.
#' It is ubiquitous and has withstood the test of time. It is not the objective
#' of [`transltr`][transltr] to (fully) replace it.
#'
#' Package [`transltr`][transltr] provides an alternative in-memory object
#' model (and further functions) to easily inspect and manipulate source text
#' and translations.
#'
#' * It does not change any aspect of the underlying locale.
#'
#' * It has its own data serialization formats for I/O purposes. Source text
#'   and translations can be exported to text formats that are sharable and
#'   easily modifiable, even by non-technical collaborators.
#'
#' * Its features are extensively documented (even internal ones).
#'
#' * It can always locate and extract translatable strings (litteral character
#'   vectors passed to [translate()]). They are treated as regular \R objects
#'   (as [`Text`][Text] objects).
#'
#' [translate()] works everywhere, including in calls to [stop()], [warning()],
#' and [message()].
#'
#' @section Getting Started:
#' Write code as you normally would. Whenever a piece of text (a literal 
#' character vector) must be available in multiple languages, wrap it with
#' [translate()].
#'
#' 1. Once you are ready to translate your project, call [find_source()].
#'    This returns a [`Translator`][Translator] object.
#'
#' 2. Export the [`Translator`][Translator] object with [translator_write()].
#'    Fill in the underlying translation files.
#'
#' 3. Import translations back into an \R session with [translator_read()].
#'
#' Current language and source language are respectively set with
#' [language_set()] and [language_source_get()]. By default, the latter is set
#' equal to `"en"` (English).
#'
#' @section Bugs and Feedback:
#' You may submit bugs, request features, and provide feedback by creating an
#' [issue on GitHub](https://github.com/jeanmathieupotvin/transltr/issues/new).
#'
#' @section Acknowledgements:
#' Warm thanks to Jérôme Lavoué, who supported and sponsored the first release
#' of this project.
#'
#' @seealso
#' The scattered and incomplete documentation of \R's Native Language Support:
#' * [gettext()],
#' * [Sys.setLanguage()], [Sys.setlocale()], [Sys.localeconv()],
#' * [tools::xgettext()],
#' * [tools::xgettext2pot()], [tools::update_pkg_po()], [tools::checkPoFiles()],
#' * [Section 3 (Internationalization)](https://cran.r-project.org/doc/manuals/r-release/R-ints.html#R-code-1)
#'    of \R Internals,
#' * [Section 7 (Internationalization and Localization)](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Internationalization-and-Localization)
#'   of \R Installation and Administration, and
#' * [Section 1.8 (Internationalization)](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Internationalization) of Writing \R Extensions.
#'
#' The comprehensive technical documentation of
#' [GNU `gettext`](https://www.gnu.org/software/gettext/manual/gettext.html).
#'
#' @keywords internal
"_PACKAGE"
