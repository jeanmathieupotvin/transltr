# active binding hash throws an error if value is not missing

    Code
      blk1$hash <- "new-hash"
    Condition
      Error:
      ! 'hash' cannot be overwritten.
      Update it by setting 'source_lang' instead.

# active binding hash_algorithm validates value

    Code
      blk1$hash_algorithm <- 1L
    Condition
      Error:
      ! 'hash_algorithm' must be a non-NA and non-empty character of length 1.

---

    Code
      blk1$hash_algorithm <- "new-algo"
    Condition
      Error:
      ! 'hash_algorithm' must be equal to 'sha1', or 'utf8'.

# active binding source_lang validates value

    Code
      blk1$source_lang <- 1L
    Condition
      Error:
      ! 'source_lang' must be a non-NA and non-empty character of length 1.

---

    Code
      blk1$source_lang <- "new-lang"
    Condition
      Error:
      ! 'source_lang' must be equal to 'en', 'es', 'fr', or 'ja'.

# active binding source_text throws an error if value is not missing

    Code
      blk1$source_text <- "new-text"
    Condition
      Error:
      ! 'source_text' cannot be overwritten.
      Update it by setting 'source_lang'.
      You may add a new translation before doing so.

# active binding languages throws an error if value is not missing

    Code
      blk1$languages <- "new-lang"
    Condition
      Error:
      ! 'languages' cannot be overwritten.
      Update them by setting, or removing translations.

# active binding translations throws an error if value is not missing

    Code
      blk1$translations <- "new-translation"
    Condition
      Error:
      ! 'translations' cannot be overwritten.
      Update them by setting, or removing translations.

# active binding locations throws an error if value is not missing

    Code
      blk1$locations <- location()
    Condition
      Error:
      ! 'locations' cannot be overwritten.
      Update them by setting, or removing 'Location' objects.

# $initialize() validates hash_algorithm

    Code
      Block$new(1L)
    Condition
      Error:
      ! 'hash_algorithm' must be equal to 'sha1', or 'utf8'.

# $get_translation() validates lang

    Code
      blk1$get_translation(1L)
    Condition
      Error:
      ! 'lang' must be a non-NA and non-empty character of length 1.

# $set_translation() validates lang

    Code
      blk1$set_translation(1L)
    Condition
      Error:
      ! 'lang' must be a non-NA and non-empty character of length 1.

# $set_translation() validates text

    Code
      blk1$set_translation("de", 1L)
    Condition
      Error:
      ! 'text' must be a non-NA character of length 1.

# $set_translations() validates ...

    Code
      blk$set_translations(1L)
    Condition
      Error:
      ! values passed to '...' must all be non-NA and non-empty character strings.

---

    Code
      blk$set_translations("Hello, world!")
    Condition
      Error:
      ! '...' must have names.

# $rm_translation() validates lang

    Code
      blk1$rm_translation(1L)
    Condition
      Error:
      ! 'lang' must be a non-NA and non-empty character of length 1.

---

    Code
      blk1$rm_translation("en")
    Condition
      Error:
      ! 'en' is the current 'source_lang'. Set a new one before removing it.

---

    Code
      blk1$rm_translation("error")
    Condition
      Error:
      ! 'lang' must be equal to 'es', 'fr', or 'ja'.

# $rm_location() validates path

    Code
      blk1$rm_location(1L)
    Condition
      Error:
      ! 'path' must be a non-NA and non-empty character of length 1.

---

    Code
      blk1$rm_location("error")
    Condition
      Error:
      ! 'path' must be equal to 'a', or 'b'.

# block() validates source_lang

    Code
      block("")
    Condition
      Error:
      ! 'source_lang' must be a non-NA and non-empty character of length 1.

# block() checks that there is at least one translation corresponding to source_lang

    Code
      block("en")
    Condition
      Error:
      ! a translation corresponding to 'source_lang' must be passed to '...'.
      It is treated as the source text.

# print() works

    Code
      print(blk1)
    Output
      <Block>
        Hash: 256e0d707386d0fcd9abf10ad994000bdaa25812
        Source Lang: en
        Algorithm: sha1
        Translations: 
          en: Hello, world!
          es: ¡Hola Mundo!
          fr: Bonjour, monde!
          ja: こんにちは世界！
        Locations: 
          <Location>
            Path: a
            Ranges: line 1, column 2 @ line 3, column 4
          <Location>
            Path: b
            Ranges: line 5, column 6 @ line 7, column 8

# c.Block() validates ...

    Code
      c(blk1, 1L, blk2)
    Condition
      Error:
      ! values passed to '...' must all be 'Block' objects.

# c.Block() throws an error if hashes are not equal

    Code
      blk1 <- test_block()
      blk2 <- test_block()
      blk2$source_lang <- "fr"
      c(blk1, blk2)
    Condition
      Error:
      ! all 'hash' must be equal in order to combine multiple 'Block' objects.

# merge_blocks() validates ...

    Code
      merge_blocks(blk1, 1L, blk2)
    Condition
      Error:
      ! values passed to '...' must all be 'Block' objects.

# merge_blocks() validates hash_algorithm

    Code
      merge_blocks(blk1, blk2, hash_algorithm = "error")
    Condition
      Error:
      ! 'hash_algorithm' must be equal to 'sha1', or 'utf8'.

# as_block.call() validates x

    Code
      as_block(call("block"))
    Condition
      Error:
      ! 'x' must be a 'call' object to 'transltr::translate()'.

# as_block.call() validates strict

    Code
      as_block(translate_call, strict = 1L)
    Condition
      Error:
      ! 'strict' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# as_block.call() validates location

    Code
      as_block(translate_call, location = 1L)
    Condition
      Error:
      ! 'location' must be a 'Location' object.

# as_block.call() validates validate

    Code
      as_block(translate_call, validate = 1L)
    Condition
      Error:
      ! 'validate' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

