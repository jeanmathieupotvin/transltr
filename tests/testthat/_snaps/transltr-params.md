# get_template_versions() returns expected values

    Code
      get_template_versions()
    Output
      [1] 1

# get_hash_algorithms() returns expected values

    Code
      get_hash_algorithms()
    Output
      [1] "blake2b"

# get_hash_length_range() returns expected values

    Code
      get_hash_length_range("blake2b")
    Output
      min max 
        8  32 

# get_hash_length_range() validates hash_algorith,

    Code
      get_hash_length_range("error")
    Condition
      Error:
      ! 'hash_algorithm' must be equal to 'blake2b'.

# get_generated_by() returns expected value

    Code
      get_generated_by()
    Output
      [1] "R package transltr 0.0.1"

