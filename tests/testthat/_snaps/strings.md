# str_strip_empty() validates x

    Code
      str_strip_empty(1L)
    Condition
      Error:
      ! 'x' must be a character vector of non-NA values.

# str_strip_empty() validates which

    Code
      str_strip_empty(which = "error")
    Condition
      Error:
      ! 'which' must be equal to 'both', 'leading', or 'trailing'.

# str_left_pad() validates x

    Code
      str_left_pad(1L)
    Condition
      Error:
      ! 'x' must be a character vector of non-NA values.

# str_left_pad() validates pad

    Code
      str_left_pad(pad = 1L)
    Condition
      Error:
      ! 'pad' must be a non-NA and non-empty character of length 1.

---

    Code
      str_left_pad(v1, pad = "aa")
    Condition
      Error:
      ! 'pad' must be a single character.

# str_left_pad() validates len

    Code
      str_left_pad(v1, len = "1")
    Condition
      Error:
      ! 'len' must be a non-NA integer of length 1.

---

    Code
      str_left_pad(v1, len = -1L)
    Condition
      Error:
      ! 'len' must be a non-NA numeric value in the range [0, Inf).

# str_trim() validates x

    Code
      str_trim(1L)
    Condition
      Error:
      ! 'x' must be a character vector of non-NA values.

# str_trim() validates len

    Code
      str_trim(v2, "1")
    Condition
      Error:
      ! 'len' must be a non-NA integer of length 1.

---

    Code
      str_trim(v2, 2L)
    Condition
      Error:
      ! 'len' must be a non-NA numeric value in the range [3, Inf).

# str_sanitize() validates x

    Code
      str_sanitize(1L)
    Condition
      Error:
      ! 'x' must be a non-empty character vector of non-NA values.

# str_sanitize() validates concat

    Code
      str_sanitize(v1, concat = 1L)
    Condition
      Error:
      ! 'concat' must be a non-NA and non-empty character of length 1.

