# active binding hash throws an error if value is not missing

    Code
      blk1$hash <- "new-hash"
    Condition
      Error:
      ! 'hash' cannot be manually overwritten. Set 'source_key' instead.

# active binding hash_algorithm validates value

    Code
      blk1$hash_algorithm <- "new-algo"
    Condition
      Error:
      ! 'hash_algorithm' must be equal to 'sha1', or 'utf8'.

# active binding source_key validates value

    Code
      blk1$source_key <- "new-key"
    Condition
      Error:
      ! 'source_key' must be equal to 'en', 'es', 'fr', or 'jp'.

# active binding source_text throws an error if value is not missing

    Code
      blk1$source_text <- "new-text"
    Condition
      Error:
      ! 'source_text' cannot be manually overwritten. Set 'source_key' instead.

# active binding keys throws an error if value is not missing

    Code
      blk1$keys <- "new-key"
    Condition
      Error:
      ! 'keys' cannot be manually overwritten.
      You may add a key with method 'set_translation()'.
      You may remove a key with method 'rm_translation()'.

# active binding translations throws an error if value is not missing

    Code
      blk1$translations <- "new-translation"
    Condition
      Error:
      ! 'translations' cannot be manually overwritten.
      You may add a translation with method 'set_translation()'.
      You may remove a translation with method 'rm_translation()'.

# active binding locations throws an error if value is not missing

    Code
      blk1$locations <- location()
    Condition
      Error:
      ! You may add a location with method 'set_location()'.
      You may remove a location with method 'rm_location()'.

# $initialize() validates hash_algorithm

    Code
      Block$new("error")
    Condition
      Error:
      ! 'hash_algorithm' must be equal to 'sha1', or 'utf8'.

# $get_translation() validates key

    Code
      blk1$get_translation(1L)
    Condition
      Error:
      ! 'key' must be a non-NA and non-empty character of length 1.

# $set_translation() validates key

    Code
      blk1$set_translation(1L)
    Condition
      Error:
      ! 'key' must be a non-NA and non-empty character of length 1.

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

# $rm_translation() validates key

    Code
      blk1$rm_translation(1L)
    Condition
      Error:
      ! 'key' must be a non-NA and non-empty character of length 1.

---

    Code
      blk1$rm_translation("en")
    Condition
      Error:
      ! 'key' 'Set a new one before removing it.' is the current 'source_key'. en

---

    Code
      blk1$rm_translation("error")
    Condition
      Error:
      ! 'key' must be equal to 'es', 'fr', or 'jp'.

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

# block() validates source_key

    Code
      block("")
    Condition
      Error:
      ! 'source_key' must be a non-NA and non-empty character of length 1.

# block() checks that there is at least one translation corresponding to source_key

    Code
      block("en")
    Condition
      Error:
      ! a translation corresponding to 'source_key' must be passed to '...'.
      It is treated as the source text.

# .block() validates source_key

    Code
      .block("")
    Condition
      Error:
      ! 'source_key' must be a non-NA and non-empty character of length 1.

# .block() validates source_text

    Code
      .block("en", 1L)
    Condition
      Error:
      ! 'source_text' must be a non-NA character of length 1.

# .block() validates trans_keys

    Code
      .block("en", "Hello, world!", trans_keys = 1L)
    Condition
      Error:
      ! 'trans_keys' must be a character vector of non-NA values.

# .block() validates trans_texts

    Code
      .block("en", "Hello, world!", trans_texts = 1L)
    Condition
      Error:
      ! 'trans_texts' must be a character vector of non-NA values.

# .block() validates lengths of trans_keys and trans_texts

    Code
      .block("en", trans_keys = "en")
    Condition
      Error:
      ! 'trans_keys' and 'trans_texts' must have the same length.

# print() works

    Code
      print(blk1)
    Output
      <Block>
        Hash: 256e0d707386d0fcd9abf10ad994000bdaa25812
        Source Key: en
        Algorithm: sha1
        Translations: 
          en: Hello, world!
          es: ¡Hola Mundo!
          fr: Bonjour, monde!
          jp: こんにちは世界！
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
      blk2$source_key <- "fr"
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
      ! 'hash_algorithm' must be equal to sha1, or utf8.

