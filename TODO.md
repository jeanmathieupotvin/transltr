# What To Do Next

> **This file is only useful for the contributors. Users should ignore it, as
> it does not constitute a formal roadmap for the project.**

# For Initial Release to CRAN

`[PT/PB/PTF]`
- **Change Translations Source File (TSF) based on Jérôme's feedback.**
  - Rename arguments `key` and `source_key` to `lang` and `source_lang`.
    - This has consequences on scripts `from-tsf.R`, and `tsf.R`.
    - This also has consequences on related test scripts.
  - Simpliy header and change overall structure.
  - Integrate class `Translator` into `from_tsf()`.
  - Revisit `split_tsf()`, `from_tsf_header()`, and `from_tsf_header_v1()`.
  - Revamp `from_tsf_block_v1()`, and lower-level mechanisms. It can be
    simplified using latest work.
  - Verify whether we could avoid tokenizing TSFs and get rid of
    `tokenize_tsf_block_v1()`.
  - Change names.
    - Rename TSF to Portable Translator File (PTF).
      - A PTF represents a Portable Translator (PT).
    - Rename source blocks to Portable Blocks (PB).
    - Rename `read_translations()` and `write_translations()` to
      `import_translator()`, and `export_translator()`.
  - Update user-level documentation on TSFs following all changes.
- Implement `to_ptf()`.
  - Write documentation and tests for it.
  - Write required lower-level functions `write_translations()`, and
    `write_text()`.
    - Write documentation and tests for it.

`[Documentation]`
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
- Revisit whether arg which of `str_strip_empty()` is required.
