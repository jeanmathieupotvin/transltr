# tsf_block_line_token() validates argument type

    Code
      tsf_block_line_token("BAD_TYPE")
    Condition
      Error:
      ! 'type' must be equal to 'NULL', 'TITLE_HASH', 'TITLE_KEY_SRC', 'TITLE_KEY_TXT', 'LOC_SRC_PATH', 'LOC_SRC_RNG', or 'TXT'.

# tsf_block_line_token() validates argument value

    Code
      tsf_block_line_token(value = 1L)
    Condition
      Error:
      ! 'value' must be a non-NA character of length 1.

# tsf_block_line_token() validates argument subtype

    Code
      tsf_block_line_token(subtype = 1L)
    Condition
      Error:
      ! 'subtype' must be a non-NA character of length 1.

