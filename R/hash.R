#' Hashing
#'
#' @description
#' Map an arbitrary character string to globally unique, and reproducible
#' output.
#'
#' **Arguments listed below are not validated for efficiency.**
#'
#' @details
#' Hashes uniquely identify the `lang` and `text` pair. Therefore, values
#' passed to these arguments are concatenated together (using `:` as the
#' separator), and [hash()] generates a a reproducible hash from the resulting
#' character string.
#'
#' [hash_algorithms()] returns available hashing algorithms. Methods are
#' described below.
#'
#' ## `SHA1`: Secure Hash Algorithm 1
#'
#' Method `sha1` corresponds to SHA-1 (Secure Hash Algorithm version 1), a
#' cryptographic hashing function. While it is now superseded by more secure
#' variants (SHA-256, SHA-512, etc.), it is still useful for non-sensitive
#' purposes. It is fast, collision-resistant, and may handle very large inputs.
#' It emits strings of 40 hexadecimal characters.
#'
#' ## `UTF8`: Cumulative UTF-8 Sum
#'
#' `r lifecycle::badge("experimental")`
#'
#' **This method is experimental. Use with caution.**
#'
#' Method `utf8` is a simple method derived from cumulative sums of UTF-8 code
#' points (converted to integers). It is slightly faster than method `sha1` for
#' small inputs, and emits shorter hashes of a variable number of digits. The
#' length is porportional to the underlying input's length.
#'
#' Strictly speaking, it is not a hashing algorithm per se. Instead, it
#' should be viewed as an identification algorithm that is highly likely to
#' produce different values for different inputs.
#'
#' @param text A non-[NA][base::NA] character string. It can be empty.
#'
#' @param algorithm A non-empty and non-[NA][base::NA] character string. The
#'   algorithm to use when hashing `lang` and `text`. See Details.
#'
#' @template param-lang
#'
#' @returns
#' [hash()] returns a character string, or `NULL` if `algorithm` is not
#' supported.
#'
#' [hash_algorithms()] returns a character vector of length 2.
#'
#' @note
#' Further algorithms such as [`xxhash`](https://github.com/Cyan4973/xxHash)
#' will be available in a near future.
#'
#' @seealso
#' [`Translator`][Translator],
#' [`Text`][Text],
#' [text_normalize()]
#'
#' @examples
#' hash("en", "Hello, world!", "sha1")     ## Outputs "256e0d707386d0fcd9abf10ad994000bdaa25812"
#' hash("en", "Hello, world!", "utf8")     ## Outputs "12351"
#' hash("en", "Hello, world!", "_error_")  ## Outputs NULL
#'
#' hash_algorithms()
#'
#' @keywords internal
#' @export
hash <- function(lang = "", text = "", algorithm = hash_algorithms()) {
    x <- sprintf("%s:%s", lang, text)

    return(
        switch(algorithm[[1L]],
            sha1 = digest::digest(charToRaw(x), algo = "sha1", serialize = FALSE),
            utf8 = as.character(sum(cumsum(utf8ToInt(x)))),
            NULL))
}

#' @rdname hash
#' @keywords internal
#' @export
hash_algorithms <- function() {
    return(c("sha1", "utf8"))
}
