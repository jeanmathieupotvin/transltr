# export.Location() validates set_uuid

    Code
      export(loc, set_uuid = 1L)
    Condition
      Error:
      ! 'set_uuid' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

