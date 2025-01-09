# str_to.default() does not quote values by default

    Code
      str_to(c(1L, 2L, 3L))
    Output
      [1] "1, 2, or 3"

---

    Code
      str_to(c(1L, 2L, 3L), quote_values = TRUE)
    Output
      [1] "'1', '2', or '3'"

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
      ! 'width' must be a non-NA integer of length 1.

---

    Code
      str_trim(v2, 2L)
    Condition
      Error:
      ! 'width' must be a non-NA numeric value in the range [3, Inf).

