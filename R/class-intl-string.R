newIntlString <- function(str = "", lang = "", loc = NULL) {
    if (!isString(str)) {
        stopf("'str' must be a non-NA character string. It can be empty.")
    }
    if (!isString(lang)) {
        stopf("'lang' must be a non-NA character string. It can be empty.")
    }
    if (!is.null(loc) && !isSrcLoc(loc)) {
        stopf("'loc' must be NULL or a 'SrcLoc' object.")
    }

    obj <- list(
        str  = str,
        hash = hashString(str),
        lang = lang,
        loc  = loc)

    class(obj) <- c("IntlString", "list")
    return(obj)
}

isIntlString <- function(x) {
    return(inherits(x, "IntlString"))
}

#' @export
format.IntlString <- function(x, indent = 0L, ...) {
    if (!isSingleIntegerInRange(indent, 0L)) {
        stopf("'indent' must be a non-NA integer value of length 1 greater than or equal to 0.")
    }
    if (is.null(x$loc)) {
        x$loc <- newSrcLoc()
    }

    str <- if (nzchar(x$str)) {
        x$str |>
        sanitizeString() |>
        strwrap(
            indent = indent + 2L,
            exdent = indent + 2L,
            width  = getOption("width", 80L))
    }

    values <- c(
        "<IntlString>" = "",
        " Hash      "  = x$hash,
        " Language  "  = if (nzchar(x$lang)) x$lang else "no language specified",
        " Location  "  = format(x$loc),
        " String    "  = if (is.null(str)) "<empty>" else "")

    return(c(sprintf("%s%s%s", strrep(" ", indent), names(values), values), str))
}

#' @export
print.IntlString <- function(x, ...) {
    cat(format(x, ...), sep = "\n")
    return(invisible(x))
}
