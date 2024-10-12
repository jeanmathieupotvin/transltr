# assert_int() adapts its error message(s)

    Code
      # Error message of assert_int() is constant.
      assert_int(1)
    Condition
      Error:
      ! '1' must be a non-empty integer vector of non-NA values.

# assert_chr() adapts its error message(s)

    Code
      assert_chr(1L)
    Condition
      Error:
      ! '1L' must be a non-empty character vector of non-NA values.

---

    Code
      assert_chr(1L, allow_empty = TRUE)
    Condition
      Error:
      ! '1L' must be a character vector of non-NA values.

# assert_lgl1() adapts its error message(s)

    Code
      # Error message of assert_lgl1() is constant.
      assert_lgl1(1)
    Condition
      Error:
      ! '1' must be a non-NA logical of length 1 ('TRUE' or 'FALSE').

# assert_int1() adapts its error message(s)

    Code
      # Error message of assert_int1() is constant.
      assert_int1(1)
    Condition
      Error:
      ! '1' must be a non-NA integer of length 1.

# assert_chr1() adapts its error message(s)

    Code
      assert_chr1(1L)
    Condition
      Error:
      ! '1L' must be a non-NA and non-empty character of length 1.

---

    Code
      assert_chr1(1L, allow_empty_string = TRUE)
    Condition
      Error:
      ! '1L' must be a non-NA character of length 1.

# assert_list() adapts its error message(s)

    Code
      assert_list(1L)
    Condition
      Error:
      ! '1L' must be a non-empty list.

---

    Code
      assert_list(1L, allow_empty = TRUE)
    Condition
      Error:
      ! '1L' must be a list.

# assert_between() adapts its error message(s)

    Code
      assert_between(0+1i)
    Condition
      Error:
      ! '0 + (0+1i)' must be a non-NA numeric value.

---

    Code
      assert_between(1L, max = 0L)
    Condition
      Error:
      ! '1L' must be a non-NA numeric value in the range (-Inf, 0].

---

    Code
      assert_between(1L, min = 2L)
    Condition
      Error:
      ! '1L' must be a non-NA numeric value in the range [2, Inf).

---

    Code
      assert_between(1L, min = 2L, max = 3L)
    Condition
      Error:
      ! '1L' must be a non-NA numeric value in the range [2, 3].

# assert_named() adapts its error message(s)

    Code
      assert_named(list(1L))
    Condition
      Error:
      ! 'list(1L)' must have names.

---

    Code
      assert_named(list(1L), allow_empty_names = TRUE)
    Condition
      Error:
      ! 'list(1L)' must have names. They can be empty strings.

---

    Code
      assert_named(list(1L), allow_na_names = TRUE)
    Condition
      Error:
      ! 'list(1L)' must have names. They can be NA values.

---

    Code
      assert_named(list(1L), allow_empty_names = TRUE, allow_na_names = TRUE)
    Condition
      Error:
      ! 'list(1L)' must have names. They can be empty strings. They can be NA values.

# assert_match() adapts its error message(s)

    Code
      # Error message of assert_match() is constant.
      assert_match(3L, c(1L, 2L))
    Condition
      Error:
      ! '3L' must be equal to 1, or 2.

# assert_match() does not quote values by default

    Code
      assert_match(3L, c(1L, 2L))
    Condition
      Error:
      ! '3L' must be equal to 1, or 2.

---

    Code
      assert_match(3L, c(1L, 2L), quote_values = TRUE)
    Condition
      Error:
      ! '3L' must be equal to '1', or '2'.

# assert_arg() adapts its error message(s)

    Code
      # Error message of assert_arg() is constant.
      wrap_assert_arg(3L)
    Condition
      Error:
      ! 'my_x' must be equal to 1, or 2.

# assert_arg() does not quote values by default

    Code
      wrap_assert_arg(3L)
    Condition
      Error:
      ! 'my_x' must be equal to 1, or 2.

---

    Code
      wrap_assert_arg(3L, quote_values = TRUE)
    Condition
      Error:
      ! 'my_x' must be equal to '1', or '2'.

