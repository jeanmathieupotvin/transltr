SrcExpr <- function(str = "", sloc = NULL) {
    assertString(str)

    if (!is.null(sloc) && !isSrcLoc(sloc)) {
        halt("'sloc' must be NULL or a 'SrcLoc' object.")
    }

    expr <- tryCatch(
        str2lang(str),
        error   = \(c) errorCondition(c$message),
        warning = \(c) warningCondition(c$message))

    status <- if (inherits(expr, "condition")) class(expr)[[1L]] else "parsed"
    return(newSrcExpr(expr, status, strhash(str), sloc))
}

newSrcExpr <- function(
    expr   = expression(),
    status = c("parsed", "error", "warning"),
    hash   = "",
    sloc   = NULL)
{
    status <- match.arg(status)
    return(structure(as.list(environment()), class = c("SrcExpr", "list")))
}

isSrcExpr <- function(x) {
    return(inherits(x, "SrcExpr"))
}

#' @export
format.SrcExpr <- function(x,
    indent = 0L,
    width  = getOption("width", 80L),
    ...)
{
    assertSingleIntInRange(indent, 0L)
    assertSingleIntInRange(width,  1L)

    x$sloc <- x$sloc %??% SrcLoc()

    # Status is either parsed or error/warning.
    expr <- if (x$status == "parsed") {
        # Max length of expr is specified width
        # minus the following reserved slots.
        # - Label: " Expression" (11 chars).
        # - Label/value separator: " " (1 char).
        # - Truncation string: "..." (3 chars).
        # - Further indentation, if any.
        trimParsedExpr(x$expr, width - indent - 15L)
    } else {
        # Extracted from print.condition().
        # There is no base::format.condition().
        # We only keep main error message and
        # drop visual location of error. Lines
        # and error location are dropped.
        gsub("\\n1:.*", "", conditionMessage(x$expr))
    }

    return(
        formatNamedValues(
            Status     = x$status,
            Hash       = x$hash,
            Location   = format(x$sloc),
            Expression = expr))
}

#' @export
print.SrcExpr <- function(x, ...) {
    cat("<SrcExpr>", format(x, ...), sep = "\n")
    return(invisible(x))
}
