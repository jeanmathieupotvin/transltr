# Support Many Languages in R

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/transltr)](https://CRAN.R-project.org/package=transltr)
[![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov](https://codecov.io/gh/jeanmathieupotvin/transltr/branch/main/graph/badge.svg?token=ODYHDNR8IB)](https://app.codecov.io/gh/jeanmathieupotvin/transltr)
[![check-standard](https://github.com/jeanmathieupotvin/transltr/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/jeanmathieupotvin/transltr/actions/workflows/check-standard.yaml)
<!-- badges: end -->

An object model for source text and translations. Find and extract translatable
strings. Provide translations and seamlessly retrieve them at runtime.

## Introduction

R relies on GNU [`gettext`](https://www.gnu.org/software/gettext/) to produce
multi-lingual messages (if *Native Language Support* is enabled). This is
well-designed software offering an extensive set of functionalities. It is
ubiquitous and has withstood the test of time. It is not the objective of 
`transltr` to (fully) replace it.

Package `transltr` provides an alternative in-memory object model (and further
functions) to easily inspect and manipulate source text and translations.

&#x2705; It does not change any aspect of the underlying locale.

&#x2705; It has its own data serialization formats for I/O purposes. Source
text and translations can be exported to text formats that are sharable and
easily modifiable, even by non-technical collaborators.

&#x2705; Its features are extensively documented (even internal ones).

&#x2705; It can always locate and extract translatable strings (no matter
where they are in the source code).

&#x2705; Translatable source text is treated as a regular R object.

## Installation

Install the package from your preferred
[CRAN mirror](https://cran.r-project.org/mirrors.html).

```r
install.packages("transltr")
```

While an extensive set of unit tests fully covers the current version of
`transltr`, some features could be modified in the future. Treat it as a
beta version until version `1.0.0` is released.

## Getting Started

Write code as you normally would. Whenever a piece of text (literal character
vectors) should be available in multiple languages, pass it to method
`Translator$translate()`. You may also use your own function.

```r
tr <- transltr::translator()

# Write code.
cat(tr$translate("Hello, world!"), "\n")
cat(tr$translate("Farewell, world!"), "\n")

# Custom functions can also be used.
cat(internationalize("Hello, world!"), "\n")
cat(internationalize("Farewell, world!"), "\n")
```

1. Once you are ready to work on translating your project, call `find_source()`.
   This returns a `Translator` object.

    ```r
    # Extract source text and create a new Translator object.
    find_source()

    # Use arg interface if a custom function was used.
    # You may also pass an existing Translator object.
    find_source(tr = tr, interface = quote(internationalize))
    find_source(tr = tr, interface = quote(pkg::internationalize))
    ```

2. Export the `Translator` object with `translator_write()`. Fill in the
   underlying translation files.

3. Import translations back into an R session with `translator_read()`.

Current language and source language are respectively set with `language_set()`
and `language_source_get()`. By default, the latter is set equal to `"en"` 
(English).

## Future Improvements

An informal and approximative roadmap for the package is
[available on GitHub](https://github.com/jeanmathieupotvin/transltr/wiki/Roadmap).

## Bugs and Feedback

You may submit bugs, request features, and provide feedback by creating an
[issue on GitHub](https://github.com/jeanmathieupotvin/transltr/issues/new).

## Acknowledgements

Warm thanks to [Jérôme Lavoué](https://orcid.org/0000-0003-4950-5475), who
gladly supported and sponsored the first release of this project.
