# strip_empty_strings() validates x

    Code
      strip_empty_strings(1L)
    Condition
      Error:
      ! 'x' must be a character vector of non-NA values.

# strip_empty_strings() validates which

    Code
      strip_empty_strings(which = "error")
    Condition
      Error:
      ! 'which' must be equal to 'both', 'leading', or 'trailing'.

# left_pad_strings() validates x

    Code
      left_pad_strings(1L)
    Condition
      Error:
      ! 'x' must be a character vector of non-NA values.

# left_pad_strings() validates pad

    Code
      left_pad_strings(pad = 1L)
    Condition
      Error:
      ! 'pad' must be a non-NA and non-empty character of length 1.

---

    Code
      left_pad_strings(v1, pad = "aa")
    Condition
      Error:
      ! 'pad' must be a single character.

# left_pad_strings() validates len

    Code
      left_pad_strings(v1, len = "1")
    Condition
      Error:
      ! 'len' must be a non-NA integer of length 1.

---

    Code
      left_pad_strings(v1, len = -1L)
    Condition
      Error:
      ! 'len' must be a non-NA numeric value in the range [0, Inf).

# trim_strings() validates x

    Code
      trim_strings(1L)
    Condition
      Error:
      ! 'x' must be a character vector of non-NA values.

# trim_strings() validates len

    Code
      trim_strings(v2, "1")
    Condition
      Error:
      ! 'len' must be a non-NA integer of length 1.

---

    Code
      trim_strings(v2, 2L)
    Condition
      Error:
      ! 'len' must be a non-NA numeric value in the range [3, Inf).

# sanitize_strings() validates x

    Code
      sanitize_strings(1L)
    Condition
      Error:
      ! 'x' must be a non-empty character vector of non-NA values.

# sanitize_strings() validates concat

    Code
      sanitize_strings(v1, concat = 1L)
    Condition
      Error:
      ! 'concat' must be a non-NA and non-empty character of length 1.

