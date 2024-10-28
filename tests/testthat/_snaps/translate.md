# translate() validates translator object it fetches for given scope

    Code
      translate("Hello, world!", scope = "error")
    Condition
      Error:
      ! no 'Translator' object set for scope 'error'. Call 'translator_set()' first.

# translate() throws an error if implicit scope has no set translator object

    Code
      evalq(envir = globalenv(), function() translate("Hello, world!", key = "fr"))()
    Condition
      Error:
      ! no 'Translator' object set for scope 'global'. Call 'translator_set()' first.

