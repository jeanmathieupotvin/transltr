newSrcExpr <- function(str = "", loc = NULL) {
    if (!isString(str)) {
        stopf("'str' must be a non-NA character string. It can be empty.")
    }
    if (!is.null(loc) && !isSrcLoc(loc)) {
        stopf("'loc' must be NULL or a 'SrcLoc' object.")
    }

    newSrcExprCond <- function(cond) {
        class(cond$message) <- "SrcExprCond"
        return(cond$message)
    }

    expr <- tryCatch(
        str2lang(str),
        error   = newSrcExprCond,
        warning = newSrcExprCond)

    obj <- list(
        expr   = expr,
        status = if (inherits(expr, "SrcExprCond")) "error" else "parsed",
        hash   = hashString(str),
        loc    = loc)

    class(obj) <- c("SrcExpr", "list")
    return(obj)
}

isSrcExpr <- function(x) {
    return(inherits(x, "SrcExpr"))
}

#' @export
format.SrcExpr <- function(x, indent = 0L, ...) {
    if (!isSingleIntegerInRange(indent, 0L)) {
        stopf("'indent' must be a non-NA integer value of length 1 greater than or equal to 0.")
    }
    if (is.null(x$loc)) {
        x$loc <- newSrcLoc()
    }

    values <- c(
        "<SrcExpr>"   = "",
        " Status    " = x$status,
        " Hash      " = x$hash,
        " Location  " = format(x$loc))

    return(sprintf("%s%s%s", strrep(" ", indent), names(values), values))
}

#' @export
print.SrcExpr <- function(x, ...) {
    cat(format(x, ...), sep = "\n")
    return(invisible(x))
}
