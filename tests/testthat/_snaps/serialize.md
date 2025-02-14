# export.Location() validates id

    Code
      export(loc1, 1L)
    Condition
      Error:
      ! 'id' must be a non-NA and non-empty character of length 1.

# export.Text() validates id

    Code
      export(txt1, 1L)
    Condition
      Error:
      ! 'id' must be a non-NA and non-empty character of length 1.

# export.Text() validates set_translations

    Code
      export(txt1, set_translations = 1L)
    Condition
      Error:
      ! 'set_translations' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# serialize() serializes objects as expected

    Code
      cat(serialize(loc1, id = "test-id"))
    Output
      !<Location>
      Identifier: test-id
      Path: a
      Ranges: Ln 1, Col 2 @ Ln 3, Col 4

---

    Code
      cat(serialize(txt1, id = "test-id"))
    Output
      !<Text>
      Identifier: test-id
      Algorithm: sha1
      Hash: 256e0d707386d0fcd9abf10ad994000bdaa25812
      Source Language: en
      Source Text: Hello, world!
      Translations: ~
      Locations:
        - !<Location>
          Identifier: test-id:l1
          Path: a
          Ranges: Ln 1, Col 2 @ Ln 3, Col 4

---

    Code
      cat(serialize(tr))
    Output
      !<Translator>
      Identifier: test-translator
      Algorithm: sha1
      Languages:
        el: Ελληνικά
        en: English
        es: Español
        fr: Français
      Texts:
        - !<Text>
          Identifier: 256e0d7
          Algorithm: sha1
          Hash: 256e0d707386d0fcd9abf10ad994000bdaa25812
          Source Language: en
          Source Text: Hello, world!
          Translations: ~
          Locations:
            - !<Location>
              Identifier: 256e0d7:l1
              Path: a
              Ranges: Ln 1, Col 2 @ Ln 3, Col 4
        - !<Text>
          Identifier: 2ac373a
          Algorithm: sha1
          Hash: 2ac373aa699a6712cdaddbead28031d537de29bc
          Source Language: en
          Source Text: Farewell, world!
          Translations: ~
          Locations:
            - !<Location>
              Identifier: 2ac373a:l1
              Path: b
              Ranges: Ln 5, Col 6 @ Ln 7, Col 8

# serialize() wraps long source text and translations

    Code
      cat(serialize(tr, set_translations = TRUE))
    Output
      !<Translator>
      Identifier: test-translator
      Algorithm: sha1
      Languages:
        en: English
        fr: Français
      Texts:
        - !<Text>
          Identifier: 818e96e
          Algorithm: sha1
          Hash: 818e96e5345d91681d5c34f26d4fd3f1846beb6a
          Source Language: en
          Source Text: |-
            Lorem Ipsum is simply dummy text of the printing and typesetting
            industry. Lorem Ipsum has been the industry's standard dummy text ever
            since the 1500s, when an unknown printer took a galley of type and
            scrambled it to make a type specimen book.
          Translations:
            fr: |-
              Le Lorem Ipsum est simplement du faux texte employé dans la composition
              et la mise en page avant impression. Le Lorem Ipsum est le faux texte
              standard de l'imprimerie depuis les années 1500, quand un imprimeur
              anonyme assembla ensemble des morceaux de texte pour réaliser un livre
              spécimen de polices de texte.
          Locations: []

# export_translations() validates lang

    Code
      export_translations(tr, 1L)
    Condition
      Error:
      ! 'lang' must be a non-NA and non-empty character of length 1.

---

    Code
      export_translations(tr, "missing-lang")
    Condition
      Error:
      ! 'lang' must have a corresponding native language registered in 'tr'.

# export_translations() validates tr

    Code
      export_translations(1L, "fr")
    Condition
      Error:
      ! 'tr' must be a 'Translator' object.

---

    Code
      export_translations(tr, "fr")
    Condition
      Error:
      ! all 'Text' objects of 'tr' must have the same 'source_lang'.

# serialize_translations() serializes translations as expected

    Code
      cat(serialize_translations(tr, "el"), "\n")
    Output
      :: Identifier
      
      test-translator:translations:el
      
      :: Language Code
      
      el
      
      :: Language
      
      Ελληνικά
      
      :: Source Language
      
      English
      
      :: Translations: 256e0d7: Source Text
      
      Hello, world!
      
      :: Translations: 256e0d7: Translation
      
      # Insert a translation here.
      
      :: Translations: 2ac373a: Source Text
      
      Farewell, world!
      
      :: Translations: 2ac373a: Translation
      
      # Insert a translation here. 

---

    Code
      cat(serialize_translations(tr, "fr"), "\n")
    Output
      :: Identifier
      
      test-translator:translations:fr
      
      :: Language Code
      
      fr
      
      :: Language
      
      Français
      
      :: Source Language
      
      English
      
      :: Translations: 256e0d7: Source Text
      
      Hello, world!
      
      :: Translations: 256e0d7: Translation
      
      Bonjour, monde!
      
      :: Translations: 2ac373a: Source Text
      
      Farewell, world!
      
      :: Translations: 2ac373a: Translation
      
      Au revoir, monde! 

# serialize_translations() wraps long source texts and translations

    Code
      cat(serialize_translations(tr, "fr"), "\n")
    Output
      :: Identifier
      
      test-translator:translations:fr
      
      :: Language Code
      
      fr
      
      :: Language
      
      Français
      
      :: Source Language
      
      English
      
      :: Translations: 818e96e: Source Text
      
      Lorem Ipsum is simply dummy text of the printing and typesetting industry.
      Lorem Ipsum has been the industry's standard dummy text ever since the 1500s,
      when an unknown printer took a galley of type and scrambled it to make a type
      specimen book.
      
      :: Translations: 818e96e: Translation
      
      Le Lorem Ipsum est simplement du faux texte employé dans la composition et la
      mise en page avant impression. Le Lorem Ipsum est le faux texte standard de
      l'imprimerie depuis les années 1500, quand un imprimeur anonyme assembla
      ensemble des morceaux de texte pour réaliser un livre spécimen de polices de
      texte. 

# format_errors() validates errors

    Code
      format_errors(1L)
    Condition
      Error:
      ! 'errors' must be a non-empty character vector of non-NA values.

# format_errors() validates throw_error

    Code
      format_errors("", throw_error = 1L)
    Condition
      Error:
      ! 'throw_error' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# format_errors() formats errors as expected

    Code
      format_errors(errors, "test-id")
    Condition
      Error:
      ! 
       - 'Hash' must be a null, or a non-empty character string.
       - 'Hash' is defined but not 'Source Text', and/or 'Source Lang'.

# assert.ExportedLocation() throws an error if x is invalid and throw_error is true

    Code
      assert(invalid)
    Condition
      Error:
      ! 
       - 'Path' must be a non-empty character string.
       - 'Ranges' must be a single `Ln <int>, Col <int> @ Ln <int>, Col <int>` character string, or a sequence of such values.

# assert.ExportedLocation() detects invalid Path field

    Code
      assert(out)
    Condition
      Error:
      ! 'Path' must be a non-empty character string.

# assert.ExportedLocation() detects invalid Ranges field

    Code
      assert(out)
    Condition
      Error:
      ! 'Ranges' must be a single `Ln <int>, Col <int> @ Ln <int>, Col <int>` character string, or a sequence of such values.

# assert.ExportedText() throws an error if x is invalid and throw_error is true

    Code
      assert(invalid)
    Condition
      Error:
      ! 
       - 'Algorithm' must be equal to 'sha1', or 'utf8'.
       - 'Hash' must be a null, or a non-empty character string.
       - 'Source Language' must be a null, or a non-empty character string.
       - 'Source Text' must be a null, or a non-empty character string.
       - 'Translations' must be a null, or a mapping of non-empty character strings.
       - 'Locations' must be a sequence of 'Location' objects.

# assert.ExportedText() detects invalid Algorithm field

    Code
      assert(out)
    Condition
      Error:
      ! 'Algorithm' must be equal to 'sha1', or 'utf8'.

# assert.ExportedText() detects invalid Hash field

    Code
      assert(out1)
    Condition
      Error:
      ! 'Hash' must be a null, or a non-empty character string.

---

    Code
      assert(out3)
    Condition
      Error:
      ! 'Hash' is defined but not 'Source Text', and/or 'Source Lang'.

# assert.ExportedText() detects invalid Source Language field

    Code
      assert(out1)
    Condition
      Error:
      ! 
       - 'Hash' is defined but not 'Source Text', and/or 'Source Lang'.
       - 'Source Language' must be a null, or a non-empty character string.
       - 'Source Language' is defined but not 'Source Text', or vice-versa.

# assert.ExportedText() detects invalid Source Text field

    Code
      assert(out1)
    Condition
      Error:
      ! 
       - 'Hash' is defined but not 'Source Text', and/or 'Source Lang'.
       - 'Source Text' must be a null, or a non-empty character string.
       - 'Source Language' is defined but not 'Source Text', or vice-versa.

---

    Code
      assert(out2)
    Condition
      Error:
      ! 'Source Text' must be a null, or a non-empty character string.

# assert.ExportedText() detects invalid Translations field

    Code
      assert(out1)
    Condition
      Error:
      ! 'Translations' must be a null, or a mapping of non-empty character strings.

# assert.ExportedText() detects invalid Locations field

    Code
      assert(out1)
    Condition
      Error:
      ! 'Locations' must be a sequence of 'Location' objects.

---

    Code
      assert(out3)
    Condition
      Error:
      ! 
       - ['test-id'] 'Path' must be a non-empty character string.
       - ['test-id'] 'Ranges' must be a single `Ln <int>, Col <int> @ Ln <int>, Col <int>` character string, or a sequence of such values.

# assert.ExportedTranslator() throws an error if x is invalid and throw_error is true

    Code
      assert(invalid)
    Condition
      Error:
      ! 
       - 'Identifier' must be a non-empty character string.
       - 'Algorithm' must be equal to 'sha1', or 'utf8'.
       - 'Languages' must a mapping of non-empty character strings.
       - 'Texts' must a sequence of 'Text' objects.

# assert.ExportedTranslator() detects invalid Identifier field

    Code
      assert(out)
    Condition
      Error:
      ! 'Identifier' must be a non-empty character string.

# assert.ExportedTranslator() detects invalid Algorithm field

    Code
      assert(out)
    Condition
      Error:
      ! 'Algorithm' must be equal to 'sha1', or 'utf8'.

# assert.ExportedTranslator() detects invalid Languages field

    Code
      assert(out1)
    Condition
      Error:
      ! 'Languages' must a mapping of non-empty character strings.

# assert.ExportedTranslator() detects invalid Texts field

    Code
      assert(out1)
    Condition
      Error:
      ! 'Texts' must a sequence of 'Text' objects.

---

    Code
      assert(out3)
    Condition
      Error:
      ! 
       - ['test-id'] 'Hash' must be a null, or a non-empty character string.
       - ['test-id'] 'Algorithm' must be equal to 'sha1', or 'utf8'.

# assert.ExportedTranslations() throws an error if x is invalid and throw_error is true

    Code
      assert(invalid)
    Condition
      Error:
      ! 
       - 'Identifier' must be a non-empty character string.
       - 'Language Code' must be a non-empty character string.
       - 'Language' must be a non-empty character string.
       - 'Source Language' must be a non-empty character string.
       - 'Translations' must be a sequence of 'Source Text', and 'Translation' sections.

# assert.ExportedTranslations() detects invalid Identifier field

    Code
      assert(out)
    Condition
      Error:
      ! 'Identifier' must be a non-empty character string.

# assert.ExportedTranslations() detects invalid Language Code field

    Code
      assert(out)
    Condition
      Error:
      ! 'Language Code' must be a non-empty character string.

# assert.ExportedTranslations() detects invalid Language field

    Code
      assert(out)
    Condition
      Error:
      ! 'Language' must be a non-empty character string.

# assert.ExportedTranslations() detects invalid Source Language field

    Code
      assert(out)
    Condition
      Error:
      ! 'Source Language' must be a non-empty character string.

# assert.ExportedTranslations() detects invalid Translations field

    Code
      assert(out1)
    Condition
      Error:
      ! 
       - 'Translations' must be a sequence of 'Source Text', and 'Translation' sections.
       - 'Translation' sections must be character strings. They can also be empty, but not missing.

---

    Code
      assert(out2)
    Condition
      Error:
      ! 'Translations' must be a sequence of 'Source Text', and 'Translation' sections.

---

    Code
      assert(out3)
    Condition
      Error:
      ! 'Source Text' sections must be character strings. They can also be empty, or missing.

# import.default() works

    Code
      import(1L)
    Condition
      Error:
      ! deserialized object is not supported by transltr. It is likely missing a '!<type>' tag, or has an invalid one.

# import.ExportedText() throws a warning if hash is invalid

    Code
      import(etxt1)
    Condition
      Warning:
      ['test-id'] 'Hash' is not equal to computed hash ('256e0d707386d0fcd9abf10ad994000bdaa25812'). The latter will be used.
    Output
      <Text>
       Hash: 256e0d707386d0fcd9abf10ad994000bdaa25812
       Source Lang: en
       Algorithm: sha1
       Translations:
        en: Hello, world!
        es: ¡Hola Mundo!
        fr: Bonjour, monde!
       Locations:
        a:
         <Location>
          Path: a
          Ranges: Ln 1, Col 2 @ Ln 3, Col 4

# deserialize() validates string

    Code
      deserialize(1L)
    Condition
      Error:
      ! 'string' must be a non-NA and non-empty character of length 1.

# deserialize() throws an error when string is an invalid yaml object

    Code
      deserialize("a: 1\nb 2\n")
    Condition
      Error:
      ! while unserializing object: scanner error: while scanning a simple key at line 2, column 1 could not find expected ':' at line 3, column 1.

# import.ExportedTranslations() validates tr if it is not null

    Code
      import(export_translations(tr, "fr"), 1L)
    Condition
      Error:
      ! 'tr' must be a 'Translator' object.

