get_template_versions <- function() {
    return(1L)
}

get_hash_algorithms <- function() {
    return("blake2b")
}

get_hash_length_range <- function(hash_algorithm = "blake2b") {
    # Internally, we use lengths in bytes.
    # 1 byte is written with 2 hexadecimal
    # characters. We also assume algorithm
    # is valid already.
    return(
        switch(hash_algorithm,
            "blake2b" = c(min = 8L, max = 32L),
            NULL))
}
