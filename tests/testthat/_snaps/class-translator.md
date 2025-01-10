# active binding id validates value

    Code
      tr1$id <- 1L
    Condition
      Error:
      ! 'id' must be a non-NA and non-empty character of length 1.

# active binding algorithm validates value

    Code
      tr1$algorithm <- 1L
    Condition
      Error:
      ! 'algorithm' must be equal to 'sha1', or 'utf8'.

---

    Code
      tr1$algorithm <- "new-algo"
    Condition
      Error:
      ! 'algorithm' must be equal to 'sha1', or 'utf8'.

# active binding hashes validates value

    Code
      tr1$hashes <- "error"
    Condition
      Error:
      ! 'hashes' cannot be overwritten.
      Update them by setting 'algorithm' and by setting, or removing 'Text' objects.

# active binding source_texts validates value

    Code
      tr1$source_texts <- 1L
    Condition
      Error:
      ! 'source_texts' cannot be overwritten.
      Update them by setting, or removing 'Text' objects.

# active binding source_langs validates value

    Code
      tr1$source_langs <- "new-source-lang"
    Condition
      Error:
      ! 'source_langs' cannot be overwritten.
      Update them by setting, or removing 'Text' objects.

# active binding languages validates value

    Code
      tr1$languages <- "error"
    Condition
      Error:
      ! 'languages' cannot be overwritten.
      Update them by setting, or removing 'Text' objects.

# active binding native_languages validates value

    Code
      tr1$native_languages <- "error"
    Condition
      Error:
      ! use 'set_native_languages()' to update 'native_languages'.

# $translate() validates source_lang

    Code
      tr1$translate(lang = "en", source_lang = 1L)
    Condition
      Error:
      ! 'source_lang' must be a non-NA and non-empty character of length 1.

# $get_text() validates hash

    Code
      tr1$get_text(hash = 1L)
    Condition
      Error:
      ! 'hash' must be a non-NA and non-empty character of length 1.

# $set_texts() validates ...

    Code
      Translator$new()$set_texts(1L, text(en = "Bye bye!"))
    Condition
      Error:
      ! values passed to '...' must all be 'Text' objects.

# $set_native_languages() validates ...

    Code
      tr1$set_native_languages(1L)
    Condition
      Error:
      ! values passed to '...' must all be character strings or 'NULL'.
      Use 'NULL' to remove entries.

---

    Code
      tr1$set_native_languages("English")
    Condition
      Error:
      ! '...' must have names.

# $rm_text() throws an error if there are no Text objects to remove

    Code
      Translator$new()$rm_text("error")
    Condition
      Error:
      ! there are no registered 'Text' objects to remove.

# $rm_text() validates hash

    Code
      tr1$rm_text(1L)
    Condition
      Error:
      ! 'hash' must be a non-NA and non-empty character of length 1.

---

    Code
      tr1$rm_text("error")
    Condition
      Error:
      ! 'hash' must be equal to '256e0d7', or '2ac373a'.

# translator() throws a warning if a language does not have a corresponding native language

    Code
      translator(id = "test-translator", en = "English", text(en = "Hello, world!",
        fr = "Bonjour, monde!"))
    Condition
      Warning:
      some languages are missing an equivalent native language name: 'fr'.
    Output
      <Translator>
       Identifier: test-translator
       Algorithm: sha1
       Languages:
        en: English
       Source Texts:
        256e0d7 [en, fr]: Hello, world!

# print() works

    Code
      print(tr1)
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

