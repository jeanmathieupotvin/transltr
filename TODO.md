# What To Do Next

> **This file is only useful for the contributors. Users should ignore it, as
> it does not constitute a formal roadmap for the project.**

# For Initial Release to CRAN

`[Portable]`
- Update documentation for classes
  - `PortableTranslator`,
  - `PortableBlock`,
  - `PortableLocation`, and
  - `PortableTranslations`.
- Update side-files used for illustration purposes.
- Write unit tests for functions
  - `text_write()`,
  - `portable()`,
  - `is_portable()`,
  - `portable_translator()`,
  - `portable_block()`,
  - `portable_location()`,
  - `portable_translations()`,
  - `format.Portable()`,
  - `format.PortableTranslator()`,
  - `format.PortableTranslations()`, and
  - `print.Portable()`, and
  - `translator_export()`.
- Finish writing functions (including documentation and unit tests).
  - `as_translator.PortableTranslator()`,
  - `as_block.PortableBlock()`,
  - `as_location.PortableLocation()`,
  - `as_translator.Translator()`,
  - `as_block.Block()`,
  - `as_location.Location()`,
  - `as_translator()`,
  - `as_block()`, and
  - `translator_import()`.

`[Utilities]`
- Write `source_language_set()`, and `source_language_get()`.
- Introduce new option `transltr.default_dir`.
- Rename class `Block` to `SourceText`.

`[Documentation]`
- Write package-level documentation (`R/transltr-package.R`).
- Run `R CMD check`.
- Remove architecture diagram.
- Write small introduction to package in package documentation.
- Final update of side-files: `DESCRIPTION`, `.Rbuildignore`, `COVERAGE`,
  `NAMESPACE`, `STATISTICS`, and `NEWS`.
  - This should be very minor changes, mostly just a final check.
- Complete top-level `README`.

# Future Improvements

`[update_translations()]`
- Implement this function that updates existing PTF.
- Implement interactive function `compare_translations()`, and related
  functions `update_show()`, `update_accept()`, and `update_reject()`.

`[text_*()]`
- Evaluate feasability of rewriting `text_normalize()` and `text_hash()` in C.
  - These functions are central to everything else and deserves the fastest implementation.
  - Evaluate which C library is the best to compute fast SHA-1 hashes.
- Revamp current implementation of `text_normalize()`.
  - Make it less reliant on `gsub()`.
  - Users should be able to pass runtime values (use `<placeholder>` in strings).
    - Example: `text_normalize("I ate", n, "bananas.") -> "I ate <placeholder:n> bananas."`
  - Users should be able to indicate text to be left as is.
    - Example: `text_normalize("I ate{{\n\n}}", "bananas.") -> "I ate\n\n bananas."`

`[PO and POT files]`
- Include full support of PO and POT files with functions such as
  - `po_read()`,
  - `po_convert()`,
  - `find_source(, gettext = TRUE)`, etc.

`[Scopes]`
- Integrate scopes into `find_source()`, and class `Block`.
- Revisit `translator_scope()`.
  - I think the call stack can be traversed more efficiently by looping
    on enclosures of evaluation frames.

`[Cache]`
- Explore the idea of replacing `.__translators_cache__` by a singleton
  instance of `TranslatorsCache` R6 class. Its interface would have two methods:
  `set()`, and `get()`, and bindings would be defined in `private`.
  - It would be instantiated once in a `zzz.R` script.
  - It would be opaque to users, just like the current implementation.

`[Documentation]`
- Review older documentation for consistency.
- Write introductory vignette.
- Build dedicated website with `pkgdown`.

`[Miscellaneous]`
- Revisit whether arg `which` of `str_strip_empty()` is required.
- `format_vector()` needs to (better) accomodate vectors of length 1.
  - It should be rewritten.
