#' Parent Directory
#'
#' Check whether the parent directory of a normalized `path` exists, and if
#' it does not, attempt to create it.
#'
#' @template param-path
#'
#' @returns
#' A character string. A [normalized][normalizePath()] and absolute path to
#' the parent directory of `path`.
#'
#' @rdname utils-create-parent-dir
#' @family utility functions
#' @keywords internal
create_parent_dir <- function(path = "") {
    assert_chr1(path)

    path       <- normalizePath(path, mustWork = FALSE)
    parent_dir <- dirname(path)

    if (!dir.exists(parent_dir) &&
        !dir.create(parent_dir, TRUE, TRUE) || .__LGL_DEBUG_FLAG) {
        stopf(
            "parent directory of path ('%s') could not be created. %s",
            "Create it manually, or change 'path'.")
    }

    return(parent_dir)
}
