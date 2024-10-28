# What To Do Next

These tasks were last updated on `October 28, 2024`.

> **This file is only useful for the maintainer and contributors.
> Regular users may safely ignore it, as it does not constitute a formal
> roadmap for the project.**

# For Initial Release to CRAN

`[Translator]`
- Write documentation and tests for class `Translator` and related features.
- Update documentation of class `Block` after writing documentation of class
  `Translator`.

`[translate()]`
- Write documentation and tests for `translate()`.
- Revisit and revamp `is_translate_call()` and `match_translate_call()`.
  - Write documentation and tests for them.
  - Revisit function `as_block.call()`. It may not be needed afterwards.
    - Update documentation and write tests if we keep it.

`[find_source()]`
- Rename `find_translations*()` to `find_source()`.
  - Integrate class `Translator` into `find_source()`.
  - Integrate changes to `is_translate_call()` and `match_translate_call()`.
  - Revisit current implementation for consistency with latest work.
  - Write documentation and tests for `find_source*()` functions.
    - Integrate local mock stored in `.local/` into `_mocks/`.

`[PT/PB/PTF]`
- **Change Translations Source File (TSF) based on Jérôme's feedback.**
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

`[Miscellaneous]`
- Change arguments `key` and `source_key` for `lang` and `source_lang`.

# Future Improvements

`[update_translations()]`
- Implement this function that updates existing PTF.
- Implement interactive function `compare_translations()`, and related
  functions `update_show()`, `update_accept()`, and `update_reject()`.

`[text_normalize()]`
- Revamp current implementation of `text_normalize()`.
  - Make it less reliant on `gsub()`.
  - Evaluate whether it should be done in C for maximum efficiency.

`[Scopes]`
- Integrate scopes into `find_source()`, and class `Block`.
- Revisit `translator_scope()`.
  - I think the call stack can be traversed more efficiently by looping
    on enclosures of evaluation frames.

`[Documentation]`
- Review older documentation for consistency.
- Write introductory vignette.
- Build dedicated website with `pkgdown`.

`[Miscellaneous]`
- Revisit whether arg which of `str_strip_empty()` is required.
