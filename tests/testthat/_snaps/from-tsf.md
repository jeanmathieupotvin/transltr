# split_tsf() validates x

    Code
      split_tsf(1L)
    Condition
      Error:
      ! 'x' must be a character vector of non-NA values.

# split_tsf() throws an error if a header's separator is missing

    Code
      split_tsf(mock_tsf_v1_no_sep_start)
    Condition
      Error:
      ! invalid or missing header. A separator could be missing.

# split_tsf() throws an error if the header is missing

    Code
      split_tsf(mock_tsf_v1_no_head)
    Condition
      Error:
      ! invalid or missing header. A separator could be missing.

# from_tsf_header() throws an error if template_version is missing

    Code
      from_tsf_header("")
    Condition
      Error:
      ! 'template_version' must be equal to 1.

# from_tsf_header() throws an error if the yaml parser fails

    Code
      from_tsf_header(mock_tsf_v1_head_dup_keys)
    Condition
      Error:
      ! header could not be read. The parser returned this error:
      ! Duplicate map key: 'en'.

# from_tsf_blocks() validates template_version

    Code
      from_tsf_blocks(template_version = "")
    Condition
      Error:
      ! 'template_version' must be equal to 1.

# from_tsf_header_v1() throws an (appropriate) error if a field is missing

    Code
      from_tsf_header_v1()
    Condition
      Error:
      ! incomplete header. These fields are required but missing: 'template_version', 'generated_by', 'generated_on', 'hash_algorithm', and 'language_keys'.

# from_tsf_header_v1() validates generated_by

    Code
      from_tsf_header_v1(template_version = 1L, generated_by = 1L, generated_on = "August 22, 2024 @ 08:00 (UTC)",
        hash_algorithm = "sha1", language_keys = list(en = "English", fr = "Français"))
    Condition
      Error:
      ! 'generated_by' must be a non-NA and non-empty character of length 1.

# from_tsf_header_v1() validates generated_on

    Code
      from_tsf_header_v1(template_version = 1L, generated_by = "R package transltr 0.0.1",
        generated_on = 1L, hash_algorithm = "sha1", language_keys = list(en = "English",
          fr = "Français"))
    Condition
      Error:
      ! 'generated_on' must be a non-NA and non-empty character of length 1.

# from_tsf_header_v1() validates hash_algorithm

    Code
      from_tsf_header_v1(template_version = 1L, generated_by = "R package transltr 0.0.1",
        generated_on = "August 22, 2024 @ 08:00 (UTC)", hash_algorithm = 1L,
        language_keys = list(en = "English", fr = "Français"))
    Condition
      Error:
      ! 'hash_algorithm' must be equal to 'sha1', or 'utf8'.

# from_tsf_header_v1() validates language_keys

    Code
      from_tsf_header_v1(template_version = 1L, generated_by = "R package transltr 0.0.1",
        generated_on = "August 22, 2024 @ 08:00 (UTC)", hash_algorithm = "sha1",
        language_keys = 1L)
    Condition
      Error:
      ! 'language_keys' must be a character vector of non-NA values.

---

    Code
      from_tsf_header_v1(template_version = 1L, generated_by = "R package transltr 0.0.1",
        generated_on = "August 22, 2024 @ 08:00 (UTC)", hash_algorithm = "sha1",
        language_keys = list("English", "Français"))
    Condition
      Error:
      ! 'language_keys' must have names.

# from_tsf_header_v1() validates further fields

    Code
      from_tsf_header_v1(template_version = 1L, generated_by = "R package transltr 0.0.1",
        generated_on = "August 22, 2024 @ 08:00 (UTC)", hash_algorithm = "sha1",
        language_keys = list(en = "English", fr = "Français"), 1L)
    Condition
      Error:
      ! all further fields (custom user's fields) must be named.

# from_tsf_block_loc_range_v1() throws an error if there are more than 4 values

    Code
      from_tsf_block_loc_range_v1(tsf_block_line_token(value = "- line  1,24 column 2 @ line 3, column 4"))
    Condition
      Error:
      ! the following source location's range could not be converted:
      '- line  1,24 column 2 @ line 3, column 4'.

# from_tsf_block_loc_range_v1() throws an error if it detects invalid values

    Code
      from_tsf_block_loc_range_v1(tsf_block_line_token(value = "- line  1, column 2+4i @ line 3, column 4"))
    Condition
      Error:
      ! the following source location's range could not be converted:
      '- line  1, column 2+4i @ line 3, column 4'.

# tokenize_tsf_block_v1() validates x

    Code
      tokenize_tsf_block_v1(1L)
    Condition
      Error:
      ! 'x' must be a character vector of non-NA values.

