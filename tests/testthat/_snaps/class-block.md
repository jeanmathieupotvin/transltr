# active binding hash throws an error if value is not missing

    Code
      blk$hash <- "new-hash"
    Condition
      Error:
      ! 'hash' cannot be manually overwritten. Set 'source_key' instead.

# active binding hash_algorithm validates value

    Code
      blk$hash_algorithm <- "new-algo"
    Condition
      Error:
      ! 'hash_algorithm' must be equal to 'sha1', or 'utf8'.

# active binding source_key validates value

    Code
      blk$source_key <- "new-key"
    Condition
      Error:
      ! 'source_key' must be equal to 'en', 'es', 'fr', or 'jp'.

# active binding source_text throws an error if value is not missing

    Code
      blk$source_text <- "new-text"
    Condition
      Error:
      ! 'source_text' cannot be manually overwritten. Set 'source_key' instead.

# active binding keys throws an error if value is not missing

    Code
      blk$keys <- "new-key"
    Condition
      Error:
      ! 'keys' cannot be manually overwritten.
      You may add a key with method 'set_translation()'.
      You may remove a key with method 'rm_translation()'.

# active binding translations throws an error if value is not missing

    Code
      blk$translations <- "new-translation"
    Condition
      Error:
      ! 'translations' cannot be manually overwritten.
      You may add a translation with method 'set_translation()'.
      You may remove a translation with method 'rm_translation()'.

# active binding locations throws an error if value is not missing

    Code
      blk$locations <- location()
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
      blk$get_translation(1L)
    Condition
      Error:
      ! 'key' must be a non-NA and non-empty character of length 1.

# $set_translation() validates key

    Code
      blk$set_translation(1L)
    Condition
      Error:
      ! 'key' must be a non-NA and non-empty character of length 1.

# $set_translation() validates text

    Code
      blk$set_translation("de", 1L)
    Condition
      Error:
      ! 'text' must be a non-NA character of length 1.

# $set_translations() validates ...

    Code
      blk$set_translations(1L)
    Condition
      Error:
      ! values passed to '...' must all be character strings.

---

    Code
      blk$set_translations("Hello, world!")
    Condition
      Error:
      ! '...' must have names.

# $rm_translation() validates key

    Code
      blk$rm_translation(1L)
    Condition
      Error:
      ! 'key' must be a non-NA and non-empty character of length 1.

---

    Code
      blk$rm_translation("en")
    Condition
      Error:
      ! 'key' 'Set a new one before removing it.' is the current 'source_key'. en

---

    Code
      blk$rm_translation("error")
    Condition
      Error:
      ! 'key' must be equal to 'es', 'fr', or 'jp'.

# $rm_location() validates path

    Code
      blk$rm_location(1L)
    Condition
      Error:
      ! 'path' must be a non-NA and non-empty character of length 1.

---

    Code
      blk$rm_location("error")
    Condition
      Error:
      ! 'path' must be equal to 'a', or 'b'.

