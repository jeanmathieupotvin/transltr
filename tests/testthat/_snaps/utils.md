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

