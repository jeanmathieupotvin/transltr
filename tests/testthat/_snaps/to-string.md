# to_string.default() does not quote values by default

    Code
      to_string(c(1L, 2L, 3L))
    Output
      [1] "1, 2, or 3"

---

    Code
      to_string(c(1L, 2L, 3L), quote_values = TRUE)
    Output
      [1] "'1', '2', or '3'"

