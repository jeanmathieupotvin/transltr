# `transltr`

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/transltr)](https://CRAN.R-project.org/package=transltr)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![Codecov](https://codecov.io/gh/jeanmathieupotvin/transltr/branch/main/graph/badge.svg?token=ODYHDNR8IB)](https://codecov.io/gh/jeanmathieupotvin/transltr)
[![check-standard](https://github.com/jeanmathieupotvin/transltr/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/jeanmathieupotvin/transltr/actions/workflows/check-standard.yaml)
<!-- badges: end -->

An alternative to `gettext()` and `xgettext()` that enables support of many
languages in any R application. Find, and extract source text that requires
translation. Store, structure, and manipulate source texts and translations
via R6 classes exposing a user-friendly API. Easily export and import to/from
a plain text format that fosters collaboration with other non-technical and
external collaborators.

## Why `transltr`?

**There is no way GNU [`gettext`](https://www.gnu.org/software/gettext/) can
be replaced.** This is well-designed software that exposes an extensive set
of functionalities. It is ubiquitous, and has withstood the test of time. Its
relevance within the R programming language is unquestionable, and it is not
the objective of `transltr` to replace it. Instead, it aims to provide a
much-lighter alternative for simpler use cases. But why?

&#x274C; Trying to extend `gettext()` to functions other than `stop()`,
`warning()`, and `message()` usually leads to fragile and incomplete
ad-hoc implementations.

&#x274C; R's Native Language Support (NLS) functionalities are not always
intuitive, and their documentation is scattered across many manual pages
of package `base` and `tools`.

&#x274C; There is no easy way to inspect, and manipulate information found in
Portable Object (`.po`) files (without writing a lot of code).

&#x274C; Portable Objects are not intuitive for non-technical collaborators.

&#x274C; Changing an R session's locale is not always a good idea when the
intent is to merely display source text in another language. Doing so may
lead to undefined behavior.

&#x274C; R offers no way to decouple languages used for backend and frontend
purposes. For example, the user interface of a [Shiny application](https://shiny.posit.co/)
could be displayed in a language that differs from the server's internal locale.

&#x2705; `transltr::translate()` works everywhere. It can always be located,
and extracted automatically with `find_source()`.

&#x2705; `transltr` features are centralized, and thoroughly documented, even
internal ones.

&#x2705; `transltr` stores, and structures source text and translations as
regular R objects. They can easily be inspected, modified, imported, and
exported.

&#x2705; `transltr` uses an intuitive plain text format that is (more) easily
sharable, maintainable, and modifiable, even by non-technical collaborators.

&#x2705; `transltr` leaves the underlying locale unchanged.

&#x2705; `transltr` may be used in conjunction with R's NLS features, keeping
front-end translations separate from back-end messages, logs, etc.

&#x2705; `transltr` only uses three carefully chosen dependencies: `digest`,
`R6`, and `yaml`.

## Installation

The plan is to submit `transltr` to CRAN for the first time in late November
2024. Until further notice, please do not attempt to use it. It should be
considered as being in a pre-beta stage until further notice.

## Usage

Write your programs as you normally would. Whenever you require a piece
of text to be translated at runtime, wrap it with `transltr::translate()`.

More to come later. See the proposed architecture below. You may also read
`Rd` files in `man/`.

## Architecture

The following diagram gives an overview of how the package works.

![](man/figures/architecture-overview-user.png)

## Bugs and feedback

Submit them [here](https://github.com/jeanmathieupotvin/transltr/issues/new).
Thank you for your collaboration.
