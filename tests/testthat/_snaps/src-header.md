# extract_src_header() throws an error if a separator is missing

    Code
      extract_src_header(mock_src_header_no_sep_start)
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

