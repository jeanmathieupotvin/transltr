# What To Do Next

> **This file is only useful for the contributors. Users should ignore it, as
> it does not constitute a formal roadmap for the project.**

# For Initial Release to CRAN

- Include examples for all exported functions.
- Write unit tests for functions
  - `constant()`,
  - `language_source_set()`,
  - `language_source_get()`,
  - `as_translator()`,
  - `as_location()`,
  - `as_location.Location()`,
  - `as_text.Text()`,
  - `text_write()`,
  - `portable()`,
  - `is_portable()`,
  - `portable_translator()`,
  - `portable_text()`,
  - `portable_location()`,
  - `portable_translations()`,
  - `format.Portable()`,
  - `format.PortableTranslator()`,
  - `format.PortableTranslations()`,
  - `print.Portable()`,
  - `as_translator.PortableTranslator()`,
  - `as_text.PortableText()`,
  - `as_location.PortableLocation()`,
  - `translator_read()`,
  - `translator_write()`,
  - `translations_read()`, and
  - `translations_write()`.
- Final update of side-files `COVERAGE`, and `STATISTICS`.

# Future Improvements

`[translator_write()]`
- Add an `overwrite` argument that is `TRUE` by default to prevent losing any
  unsaved translations. This can happen by writing over an existing set of PTFs
  before reading them first.

`[update_translations()]`
- Implement this function that updates existing PTF.
- Implement interactive function `compare_translations()`, and related
  functions `update_show()`, `update_accept()`, and `update_reject()`.

`[translate()]`
- It should be able to handle `sprintf()` placeholders natively.
- It should be able to handle plural formats natively via an intuitive notation.

`[text_normalize()]`
- Evaluate feasability of rewriting `text_normalize()` in C.
  - These functions are central to everything else and deserves the fastest implementation.
  - Evaluate which C library is the best to compute fast SHA-1 hashes.
- Revamp current implementation of `text_normalize()`.
  - Make it less reliant on `gsub()`.
  - Users should be able to pass runtime values (use `<placeholder>` in strings).
    - Example: `text_normalize("I ate", n, "bananas.") -> "I ate <placeholder:n> bananas."`
  - Users should be able to indicate text to be left as is.
    - Example: `text_normalize("I ate{{\n\n}}", "bananas.") -> "I ate\n\n bananas."`

`[text_hash()]`
- Evaluate feasability of rewriting `text_hash()` in C.
- Offer new hashing algorithms that are faster, such as `xxhash`.

`[PO and POT files]`
- Include full support of PO and POT files with functions such as
  - `po_read()`,
  - `po_convert()`,
  - `find_source(, gettext = TRUE)`, etc.

`[Scopes]`
- Integrate scopes into `find_source()`, and class `Text`.
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
- Provide translations for the package itself using `transltr`.
