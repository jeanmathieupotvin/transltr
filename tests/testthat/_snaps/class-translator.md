# active binding id validates value

    Code
      trans1$id <- 1L
    Condition
      Error:
      ! 'id' must be a non-NA and non-empty character of length 1.

# active binding hash_algorithm validates value

    Code
      trans1$hash_algorithm <- 1L
    Condition
      Error:
      ! 'hash_algorithm' must be a non-NA and non-empty character of length 1.

---

    Code
      trans1$hash_algorithm <- "new-algo"
    Condition
      Error:
      ! 'hash_algorithm' must be equal to 'sha1', or 'utf8'.

# active binding hashes validates value

    Code
      trans1$hashes <- "error"
    Condition
      Error:
      ! 'hashes' cannot be overwritten.
      Update them by setting 'hash_algorithm', and by setting, or removing 'Block' objects.

# active binding source_texts validates value

    Code
      trans1$source_texts <- 1L
    Condition
      Error:
      ! 'source_texts' cannot be overwritten.
      Update them by setting, or removing 'Block' objects.

# active binding languages validates value

    Code
      trans1$languages <- "error"
    Condition
      Error:
      ! 'languages' cannot be overwritten.
      Update them by setting, or removing 'Block' objects.

# active binding native_languages validates value

    Code
      trans1$native_languages <- "error"
    Condition
      Error:
      ! use 'set_native_languages()' to update 'native_languages'.

# $initialize() validates id

    Code
      Translator$new(1L)
    Condition
      Error:
      ! 'id' must be a non-NA and non-empty character of length 1.

# $initialize() validates hash_algorithm

    Code
      Translator$new(hash_algorithm = 1L)
    Condition
      Error:
      ! 'hash_algorithm' must be equal to 'sha1', or 'utf8'.

# $translate() validates lang

    Code
      trans1$translate(lang = 1L)
    Condition
      Error:
      ! 'lang' must be a non-NA and non-empty character of length 1.

# $translate() validates concat

    Code
      trans1$translate(lang = "en", concat = 1L)
    Condition
      Error:
      ! 'concat' must be a non-NA character of length 1.

# $translate() validates source_lang

    Code
      trans1$translate(lang = "en", source_lang = 1L)
    Condition
      Error:
      ! 'source_lang' must be a non-NA and non-empty character of length 1.

# $get_block() validates hash

    Code
      trans1$get_block(hash = 1L)
    Condition
      Error:
      ! 'hash' must be a non-NA and non-empty character of length 1.

# $set_blocks() validates ...

    Code
      Translator$new()$set_blocks(1L, block(en = "Bye bye!"))
    Condition
      Error:
      ! values passed to '...' must all be 'Block' objects.

# $set_native_languages() validates ...

    Code
      trans1$set_native_languages(1L)
    Condition
      Error:
      ! values passed to '...' must all be character strings or 'NULL'.
      Use 'NULL' to remove entries.

---

    Code
      trans1$set_native_languages("English")
    Condition
      Error:
      ! '...' must have names.

# $rm_block() throws an error if there are no Block objects to remove

    Code
      Translator$new()$rm_block("error")
    Condition
      Error:
      ! there are no registered 'Block' objects to remove.

# $rm_block() validates hash

    Code
      trans1$rm_block(1L)
    Condition
      Error:
      ! 'hash' must be a non-NA and non-empty character of length 1.

---

    Code
      trans1$rm_block("error")
    Condition
      Error:
      ! 'hash' must be equal to '256e0d7', or '2ac373a'.

# print() works

    Code
      print(trans1)
    Output
      <Translator>
        Identifier: test-translator
        Algorithm: sha1
        Languages: 
          en: English
          fr: Français
        Source Texts: 
          256e0d7 [en, fr]: Hello, world!
          2ac373a [en, fr]: Farewell, world!

