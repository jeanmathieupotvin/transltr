# _tag() validates x

    Code
      flat_tag(1L)
    Condition
      Error:
      ! 'x' must be a list.

# _tag() validates tag_sep

    Code
      flat_tag(tag_sep = 1L)
    Condition
      Error:
      ! 'tag_sep' must be a non-NA and non-empty character of length 1.

# _tag() validates tag_empty

    Code
      flat_tag(tag_empty = 1L)
    Condition
      Error:
      ! 'tag_empty' must be a non-NA character of length 1.

# _format() validates x

    Code
      flat_format(1L)
    Condition
      Error:
      ! 'x' must be a list.

# _example() prints the example before returning

    Code
      flat_example()
    Output
      # The FLAT (Flat List As Text) Format
      #
      # What follows after an octothorpe is a comment and is ignored.
      
      :: Tags: Introduction
      
      Tags begin by two colons and starts a new section.
      
      :: Tags: Sections
      
      This is a section. It contains a textual representation of a child element.
      
      :: Tags: Creating Them
      
      Tags are constructed from names extracted from the original object.
      
      :: Tags: [4]
      
      Missing names are substituted by a numbered standard label automatically.
      
      :: Comments
      
      What follows after an octothorpe is treated as a comment, and is ignored.
      # This is a comment.                                                     
      Inline comments are also allowed. # This is a comment.                   
      Escape octothorpes (\#) to treat them as regular characters.            

