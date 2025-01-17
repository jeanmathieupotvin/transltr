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
          Identifier: test-id:a
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
              Identifier: 256e0d7:a
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
              Identifier: 2ac373a:b
              Path: b
              Ranges: Ln 5, Col 6 @ Ln 7, Col 8

