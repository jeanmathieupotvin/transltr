# text_normalize() works as expected

    Code
      cat(x1, "\n")
    Output
      
              Lorem Ipsum is simply dummy text of the printing and typesetting industry.
      
              Lorem Ipsum has been the industry's standard dummy text ever since the
              1500s, when an unknown printer    took a galley of type and scrambled it
              to make a type specimen book. 
    Code
      cat(text_normalize(x1), "\n")
    Output
      Lorem Ipsum is simply dummy text of the printing and typesetting industry.
      Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. 

---

    Code
      cat(x2, "\n")
    Output
       Lorem Ipsum is simply dummy text of the printing and typesetting industry.  Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer    took a galley of type and scrambled it to make a type specimen book. 
    Code
      cat(text_normalize(x2), "\n")
    Output
      Lorem Ipsum is simply dummy text of the printing and typesetting industry.
      Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. 

---

    Code
      cat(x3, "\n")
    Output
      
              Lorem Ipsum is simply dummy text of the printing and typesetting industry.
      
                  Lorem Ipsum has been the industry's standard dummy text ever
                  since the 1500s, when an unknown printer took a galley of type
                  and scrambled it to make a type specimen book. It has survived
                  not only five centuries, but also the leap into electronic
                  typesetting, remaining essentially unchanged. It was popularised
                  in the 1960s with the release of Letraset sheets containing Lorem
                  Ipsum passages, and more recently with desktop publishing software
                  like Aldus PageMaker including versions of Lorem Ipsum. 
    Code
      cat(text_normalize(x3), "\n")
    Output
      Lorem Ipsum is simply dummy text of the printing and typesetting industry.
      Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum. 

# text_read() validates path

    Code
      text_read(1L)
    Condition
      Error:
      ! 'path' must be a non-NA and non-empty character of length 1.

---

    Code
      text_read("a-non-existent-file.txt")
    Condition
      Error:
      ! 'path' does not exist, is a directory, or is not readable.

# text_read() validates encoding

    Code
      text_read(mock_file_utf8_path, "")
    Condition
      Error:
      ! 'encoding' must be a non-NA and non-empty character of length 1.

