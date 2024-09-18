# extract_src_header() throws an error if a separator is missing

    Code
      extract_src_header(mock_src_header_no_sep_start)
    Condition
      Error:
      ! header's format is invalid. It misses a separator ('---').
      Each separator must be on its own line to be detected.

---

    Code
      extract_src_header(mock_src_header_no_sep_end)
    Condition
      Error:
      ! header's format is invalid. It misses a separator ('---').
      Each separator must be on its own line to be detected.

# extract_src_header() throws an error if header is missing

    Code
      extract_src_header(mock_src_header_no_header)
    Condition
      Error:
      ! a header is always required. Regenerate the underlying file.

# from_src_header() throws an error if it detects duplicated yaml map keys

    Code
      from_src_header(test_src_head)
    Condition
      Error:
      ! header could not be read. The parser returned this error:
      ! Duplicate map key: 'en'.

# from_src_header() throws an error if template_version is missing

    Code
      from_src_header(test_src_head)
    Condition
      Error:
      ! 'template_version' must be equal to 1.

# from_src_header_v1() validates argument generated_by

    Code
      from_src_header_v1(generated_by = 1L)
    Condition
      Error:
      ! 'generated_by' must be a non-NA and non-empty character of length 1.

# from_src_header_v1() validates argument generated_on

    Code
      from_src_header_v1(generated_on = 1L)
    Condition
      Error:
      ! 'generated_on' must be a non-NA and non-empty character of length 1.

# from_src_header_v1() validates argument hash_algorithm

    Code
      from_src_header_v1(hash_algorithm = 1L)
    Condition
      Error:
      ! 'hash_algorithm' must be equal to 'blake2b'.

# from_src_header_v1() validates argument hash_length

    Code
      from_src_header_v1(hash_length = "")
    Condition
      Error:
      ! 'hash_length' must be a non-NA integer of length 1.

---

    Code
      from_src_header_v1(hash_length = 1L)
    Condition
      Error:
      ! 'hash_length' must be a non-NA numeric value in the range [8, 32].

# from_src_header_v1() validates argument language_keys

    Code
      from_src_header_v1(language_keys = 1L)
    Condition
      Error:
      ! 'language_keys' must be a character vector of non-NA values.

---

    Code
      from_src_header_v1(language_keys = "English")
    Condition
      Error:
      ! 'language_keys' must have names.

# from_src_header_v1() validates further fields' names

    Code
      from_src_header_v1(template_version = 1L, generated_by = "R package transltr 0.0.1",
        generated_on = "August 22, 2024 @ 08:00 (UTC)", hash_algorithm = "blake2b",
        hash_length = 32L, language_keys = list(en = "English", fr = "Français"), 1L)
    Condition
      Error:
      ! all further fields (custom user's fields) must be named.

