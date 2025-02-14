#' @param interface A [`name`][name], a [`call`][call] object, or a `NULL`.
#'   A reference to an alternative (custom) function used to translate text.
#'   If a [`call`][call] object is passed to `interface`, it **must** be to
#'   operator `::`. Calls to method [`Translator$translate()`][Translator]
#'   are ignored and calls to `interface` are extracted instead. See Details
#'   below.
