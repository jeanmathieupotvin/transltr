# A Light Internationalization Framework for R

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/transltr)](https://CRAN.R-project.org/package=transltr)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![Codecov](https://codecov.io/gh/jeanmathieupotvin/transltr/branch/main/graph/badge.svg?token=ODYHDNR8IB)](https://app.codecov.io/gh/jeanmathieupotvin/transltr)
[![check-standard](https://github.com/jeanmathieupotvin/transltr/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/jeanmathieupotvin/transltr/actions/workflows/check-standard.yaml)
<!-- badges: end -->

Package `transltr` is a light, in-memory internationalization (`i18n`) R
framework. It aims to provide a flexible alternative to R's Native Language
Support (NLS) for simpler use cases.

## Introduction

By default, R fully supports `i18n`, native languages, and locales via GNU
[`gettext`](https://www.gnu.org/software/gettext/).

**It cannot be replaced.** This is well-designed software that exposes an
extensive set of functionalities. It is ubiquitous and has withstood the test
of time. Its relevance within the R programming language is unquestionable, and
it is not the objective of `transltr` to fully replace it.

### Why `transltr`?

A simplified and modern approach can be beneficial.

&#x274C; Trying to extend `gettext()` to functions other than `stop()`,
`warning()`, and `message()` may lead to fragile, incomplete, untested,
undocumented, or ad-hoc implementations.

&#x274C; R's Native Language Support functionalities are not always intuitive,
and their documentation is scattered across many manuals and help pages. It has
not aged well and needs to be more exhaustive.

&#x274C; There is no *easy* way to inspect and manipulate information stored in
[Portable Object files](https://www.gnu.org/software/gettext/manual/html_node/PO-Files.html)
(`.po` and `.pot`).

&#x274C; Portable Objects could be more intuitive for non-technical
collaborators. They were not designed for them, and their format is *crude*,
at best.

&#x274C; Changing aspects of an \R session's locale may not be a good idea when
the intent is to display source text in another language. Doing so may lead to
undefined behaviour.

&#x274C; R offers no way to decouple languages used for *backend* (internal)
and *frontend* (exported) purposes. For example, the user interface of a
[Shiny application](https://shiny.posit.co/) could be displayed in a language
that differs from the server's internal locale.

### A Fresh Approach

`transltr` attempts to solve these *incompletenesses*.

&#x2705; `translate()` works everywhere, in any function and in any context
(including in calls to `stop()`, `warning()`, etc.).

&#x2705; Features are extensively documented (even internal ones).

&#x2705; Like `gettext()`, calls to `translate()`, and the underlying source
text can always be located, extracted, and treated as a regular R object. As
such, it can be inspected, modified, imported, and exported.

&#x2705; Source text and translations are exported to a new format that is
(more) easily sharable and modifiable, even by non-technical collaborators.

&#x2705; The locale is left as is. It may still be changed if required.

## Installation

Install the package from your preferred
[CRAN mirror](https://cran.r-project.org/mirrors.html).

```r
install.packages("transltr")
```

## Getting Started

Follow these steps.

1. Develop and write code as you normally would. Whenever a piece of text (a
   **literal** character vector) must support multiple languages, wrap it
   inside a call to `translate()`.

2. Once you are ready to work on translations, call `find_source()`. This
   function returns a `Translator` object.

3. Export it with `translator_write()`. Complete the underlying
   *Portable Translations Files* with translations.

4. Import translations back into an R session with `translator_read()`.

5. Set the default language with `language_set()`.

You may specify a default (global) source language with
`language_source_get()`.

## Bugs and Feedback

You may submit bugs, request features, and provide feedback by creating an
[issue on GitHub](https://github.com/jeanmathieupotvin/transltr/issues/new).

## Acknowledgements

Warm thanks to [Jérôme Lavoué](https://orcid.org/0000-0003-4950-5475), who
supported and sponsored the `0.0.1` release (the first release) of this project
and believes in free and open-source software.
