# What To Do Next

> **This file is only useful for the contributors. Users should ignore it, as
> it does not constitute a formal roadmap for the project.**

# For Initial Release to CRAN

`[Portable]`
- Get feedback from Jérôme on new design for portability.
- Write new generic function `as_portable()`, and related S3 methods.
  - Write S3 method `as_portable.Translator()`.
  - Write S3 method `as_portable.Block()`.
- Write S3 generic function `as_translator()`.
  - Write S3 method `as_translator.PortableTranslator()`.
- Write new S3 method `as_block.PortableBlock()`.
- Write documenbtation on Portable Objects: Portable Translators, Portable
  Blocks, and Portable Translations.

`[Documentation]`
- Write package-level documentation (`R/transltr-package.R`).
- Run `R CMD check`.
- Update architecture diagram.
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

`[text_normalize()]`
- Revamp current implementation of `text_normalize()`.
  - Make it less reliant on `gsub()`.
  - Evaluate whether it should be done in C for maximum efficiency.
  - Users should be able to pass runtime values (use `<placeholder>` in strings).
    - Example: `text_normalize("I ate", n, "bananas.") -> "I ate <placeholder:n> bananas."`
  - Users should be able to indicate text to be left as is.
    - Example: `text_normalize("{{I ate\n\n}}", "bananas.") -> "I ate\n\n bananas."`

`[PO and POT files]`
- Include full support of PO and POT files.
  - `po_read()`
  - `po_convert()`

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
