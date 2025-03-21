hash_str <- function(x = "") {
    return(xxhashlite::xxhash_raw(x, "xxh32"))
}

hash_dots <- function(...) {
    return(xxhashlite::xxhash(unname(list(...)), "xxh32"))
}

hash_std_template <- function(algo = "xxh32") {
    assert_chr1(algo)
    return(
        switch(algo,
            xxh32 = c(size = 8L, regex = "[a-fA-F0-9]{8}"),
            # Other algorithms are not supported
            # and resulting hashes are invalid.
            stops("'algo' must be eqqual to 'xxh32'.")
        )
    )

}

is_hash <- function(x = "", algo = "xxh32") {
    return(is_chr1(x) && grepl(std_hash(algo)[["regex"]], x))
}
