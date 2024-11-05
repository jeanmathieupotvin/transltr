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

# format_vector() indents values

    Code
      cat(out, sep = "\n")
    Output
        a: 1
        b: 
          c: 2
          d: 
            e: 3
            f: 4

# format_vector() formats embedded structures accordingly

    Code
      cat(format_vector(struct, "<JohnDoe>"), sep = "\n")
    Output
      <JohnDoe>
        FirstName: John
        LastName: Doe
        Address: 
          StreetAddress: 123 Main Street
          City: Montreal
          Province: Quebec
          PostalCode: H0H 0H0
        Notes: 
          <nokey>: Send mail to
          <nokey>: address above.

---

    Code
      cat(format_vector(struct, "<JohnDoe>", .show_nokey = FALSE), sep = "\n")
    Output
      <JohnDoe>
        FirstName: John
        LastName: Doe
        Address: 
          StreetAddress: 123 Main Street
          City: Montreal
          Province: Quebec
          PostalCode: H0H 0H0
        Notes: 
          Send mail to
          address above.

