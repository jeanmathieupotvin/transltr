#' Hashing algorithms
#'
#' @description
#' [`transltr`][transltr] uses hashing algorithms to create globally unique
#' and reproducible identifiers from source texts. Two methods are available.
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
#' @returns
#' A character vector of length 2.
#'
#' @note
#' Further methods such as [`xxhash`](https://github.com/Cyan4973/xxHash) will
#' become available in the future.
#'
#' @seealso
#' [`Translator`][Translator],
#' [`Text`][Text]
#'
#' @export
#' @keywords internal
hash_algorithms <- function() {
    return(c("sha1", "utf8"))
}
