token <- function(type = NULL, value = NULL, ..., super = NULL) {
    if (!is.null(super)) assert_chr1(super)
    return(
        structure(
            list(type = type, value = value, ...),
            class = c(super, "Token", "list")))
}
