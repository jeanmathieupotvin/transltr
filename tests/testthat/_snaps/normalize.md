# it validates concat

    Code
      normalize(concat = 1L)
    Condition
      Error:
      ! 'concat' must be a non-NA character of length 1.

# it works as expected

    Code
      cat(x1, "\n")
    Output
      
              Lorem Ipsum is simply dummy text of the printing and typesetting industry.
      
              Lorem Ipsum has been the industry's standard dummy text ever since the
              1500s, when an unknown printer    took a galley of type and scrambled it
              to make a type specimen book. 
    Code
      cat(normalize(x1), "\n")
    Output
      Lorem Ipsum is simply dummy text of the printing and typesetting industry.
      Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. 

---

    Code
      cat(x2, "\n")
    Output
       Lorem Ipsum is simply dummy text of the printing and typesetting industry.  Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer    took a galley of type and scrambled it to make a type specimen book. 
    Code
      cat(normalize(x2), "\n")
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
      cat(normalize(x3), "\n")
    Output
      Lorem Ipsum is simply dummy text of the printing and typesetting industry.
      Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum. 

