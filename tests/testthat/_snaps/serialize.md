# export.Text() validates set_uuid

    Code
      export(txt, set_uuid = 1L)
    Condition
      Error:
      ! 'set_uuid' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# export.Text() validates set_translations

    Code
      export(txt, set_translations = 1L)
    Condition
      Error:
      ! 'set_translations' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# export.Location() validates set_uuid

    Code
      export(loc, set_uuid = 1L)
    Condition
      Error:
      ! 'set_uuid' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

