# stops() does not return the call as part of the error message

    Code
      wrap_stops <- (function() stops("this is an error message."))
      wrap_stops()
    Condition
      Error:
      ! this is an error message.

# stopf() works

    Code
      stopf("this '%s' becomes part of the error message.", "placeholder")
    Condition
      Error:
      ! this 'placeholder' becomes part of the error message.

# strip_empty_strings() validates argument x

    Code
      strip_empty_strings(1L)
    Condition
      Error:
      ! 'x' must be a character vector of non-NA values.

# strip_empty_strings() validates argument which

    Code
      strip_empty_strings(which = "error")
    Condition
      Error:
      ! 'which' must be equal to 'both', 'leading' or 'trailing'.

