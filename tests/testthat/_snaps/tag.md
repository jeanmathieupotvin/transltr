# tag() and untag() works properly

    Strings to be tagged and untagged.
    
    [1] ""                                                
    [2] " "                                               
    [3] "{{Tagged string.}}"                              
    [4] "{{ Tagged string.\n With a special character. }}"
    [5] "{{  Tagged string with inner spaces.  }}"        
    [6] "  {{    Tagged string with outer spaces.    }}  "
    [7] "\" {{ Tagged string with double quotes. }} \""   

---

    Strings normalized and tagged.
    
    [1] ""                                                
    [2] "{{   }}"                                         
    [3] "{{ Tagged string. }}"                            
    [4] "{{ Tagged string.\n With a special character. }}"
    [5] "{{ Tagged string with inner spaces. }}"          
    [6] "{{ Tagged string with outer spaces. }}"          
    [7] "{{ Tagged string with double quotes. }}"         

---

    Strings untagged. Double quotes are kept.
    
    [1] ""                                          
    [2] " "                                         
    [3] "Tagged string."                            
    [4] "Tagged string.\n With a special character."
    [5] "Tagged string with inner spaces."          
    [6] "Tagged string with outer spaces."          
    [7] "\"Tagged string with double quotes.\""     

---

    Strings untagged. Double quotes are dropped.
    
    [1] ""                                          
    [2] " "                                         
    [3] "Tagged string."                            
    [4] "Tagged string.\n With a special character."
    [5] "Tagged string with inner spaces."          
    [6] "Tagged string with outer spaces."          
    [7] "Tagged string with double quotes."         

# tag() validates strings

    TypeError: `strings` must be a character vector of non-NA values.

# tag() validates normalize

    TypeError: `normalize` must be a non-NA integer value.

# untag() validates strings

    TypeError: `strings` must be a character vector of non-NA values.

# untag() validates keepDoubleQuotes

    TypeError: `keepDoubleQuotes` must be a non-NA integer value.
