# new_block() validates argument hash

    Code
      new_block(1L)
    Condition
      Error:
      ! 'hash' must be a non-NA and non-empty character of length 1.

# new_block() validates argument translations

    Code
      new_block(test_hash, 1L)
    Condition
      Error:
      ! 'translations' must be a non-empty character vector of non-NA values.

---

    Code
      new_block(test_hash, c("fr", "en"))
    Condition
      Error:
      ! 'translations' must have names.

