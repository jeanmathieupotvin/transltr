#' @title
#' A Light Internationalization Framework for R
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' An alternative to [base::gettext()], [tools::xgettext()], and other related
#' \R features. Incorporate translations and support many languages in any \R
#' application while keeping the locale unchanged. Find, extract, structure,
#' and manipulate source text. Export it to a textual format that fosters
#' collaboration. Complement it with translations and import everything back
#' into \R sessions.
#'
#' Package [`transltr`][transltr] is a light, in-memory internationalization
#' (`i18n`) \R framework. It aims to provide a flexible alternative to \R's
#' Native Language Support (NLS) for simpler use cases.
#'
#' @section Introduction:
#' By default, \R fully supports `i18n`, native languages, and locales via GNU
#' [`gettext`](https://www.gnu.org/software/gettext/).
#'
#' **It cannot be replaced.** This is well-designed software that exposes an
#' extensive set of functionalities. It is ubiquitous and has withstood the
#' test of time. Its relevance within the \R programming language is
#' unquestionable, and it is not the objective of [`transltr`][transltr] to
#' fully replace it.
#'
#' ## Why `transltr`?
#'
#' A simplified and modern approach can be beneficial.
#'
#' 1. Trying to extend [gettext()] to functions other than [stop()],
#'    [warning()], and [message()] may lead to fragile, incomplete, untested,
#'    undocumented, or ad-hoc implementations.
#'
#' 2. \R's Native Language Support functionalities are not always intuitive,
#'    and their documentation is scattered across many manuals and help pages.
#'    It has not aged well and needs to be more exhaustive.
#'
#' 3. There is no *easy* way to inspect and manipulate information stored in
#'    [Portable Object files](https://www.gnu.org/software/gettext/manual/html_node/PO-Files.html)
#'    (`.po` and `.pot`).
#'
#' 4. Portable Objects could be more intuitive for non-technical collaborators.
#'    They were not designed for them, and their format is *crude*, at best.
#'
#' 5. Changing aspects of an \R session's locale may not be a good idea when
#'    the intent is to display source text in another language. Doing so may
#'    lead to undefined behaviour.
#'
#' 6. \R offers no way to decouple languages used for *backend* (internal) and
#'    *frontend* (exported) purposes. For example, the user interface of a
#'    [Shiny application](https://shiny.posit.co/) could be displayed in a
#'    language that differs from the server's internal locale.
#'
#' ## A Fresh Approach
#'
#' [`transltr`][transltr] attempts to solve these *incompletenesses*.
#'
#' 1. [translate()] works everywhere, in any function and in any context
#'    (including in calls to [stop()], [warning()], etc.).
#'
#' 2. Features are extensively documented (even internal ones). For example,
#'    see [export()].
#'
#' 3. Like [gettext()], calls to [translate()] and the underlying source text
#'    can always be located, extracted, and treated as a regular \R object. As
#'    such, it can be inspected, modified, imported, and exported.
#'
#' 4. Source text and translations are exported to a new format that is (more)
#'    easily sharable and modifiable, even by non-technical collaborators.
#'
#' 5. The locale is left as is. It may still be changed if required.
#'
#' @section Getting Started:
#' Follow these steps.
#'
#' 1. Develop and write code as you normally would. Whenever a piece of text
#'    (a **literal** character vector) must support multiple languages, wrap
#'    it inside a call to [translate()].
#'
#' 2. Once you are ready to work on translations, call [find_source()]. This
#'    function returns a [`Translator`][Translator] object.
#'
#' 3. Export it with [translator_write()]. Complete the underlying
#'    [Portable Translations Files][translator_read()] with translations.
#'
#' 4. Import translations back into an \R session with [translator_read()].
#'
#' 5. Set the default language with [language_set()].
#'
#' You may specify a default (global) source language with
#' [language_source_get()].
#'
#' @section Bugs and Feedback:
#' You may submit bugs, request features, and provide feedback by creating an
#' [issue on GitHub](https://github.com/jeanmathieupotvin/transltr/issues/new).
#'
#' @section Acknowledgements:
#' Warm thanks to Jérôme Lavoué, who supported and sponsored the `0.0.1`
#' release (the first release) of this project and believes in free and
#' open-source software.
#'
#' @seealso
#' The scattered and incomplete documentation of \R's Native Language Support:
#' * [gettext()],
#' * [Sys.setLanguage()], [Sys.setlocale()], [Sys.localeconv()],
#' * [tools::xgettext()],
#' * [tools::xgettext2pot()], [tools::update_pkg_po()], [tools::checkPoFiles()],
#' * [Section 3 (Internationalization)](https://cran.r-project.org/doc/manuals/r-release/R-ints.html#R-code-1)
#'    of R Internals,
#' * [Section 7 (Internationalization and Localization)](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Internationalization-and-Localization)
#'   of R Installation and Administration, and
#' * [Section 1.8 (Internationalization)](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Internationalization) of Writing R Extensions.
#'
#' The comprehensive technical documentation of
#' [GNU `gettext`](https://www.gnu.org/software/gettext/manual/gettext.html).
#'
#' @keywords internal
"_PACKAGE"


# Suppress R CMD check notes ---------------------------------------------------


#' @importFrom digest sha1
#' @importFrom R6 R6Class
#' @importFrom yaml yaml.load
#' @importFrom yaml as.yaml
NULL
