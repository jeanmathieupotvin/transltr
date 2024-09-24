#' Create file paths to mock files
#'
#' Construct file paths to mock files (files stored in `tests/testthat/_mocks`)
#' and validate them.
#'
#' [get_mock_path()] further checks whether the file exists or not. It also
#' ensures it is not a directory.
#'
#' @param name A character string. The name of the mock file. Do not include
#'   its extension. See argument `ext`.
#'
#' @param ext A character string. The mock file's extension. Do not include
#'   the usual dot, unless you are working with fancy file extensions (and
#'   it is necessary for some odd reason).
#'
#' @returns A character string.
#'
#' @keywords internal
get_mock_path <- function(name = "", ext = "R") {
    assert_chr1(name)
    assert_chr1(ext)

    file_name <- paste0(name, ".", ext, collapse = NULL)

    # This is not 100% safe but it should be enough for
    # most circumstances. We construct a relative path
    # from the current (working) directory to _mocks/.
    rel_test_dir <- if (endsWith(wd <- getwd(), "testthat")) {
        file.path(".", "_mocks")
    } else {
        file.path("tests", "testthat", "_mocks")
    }

    # normalizePath() is not super
    # useful, but still a little safer.
    path <- normalizePath(file.path(rel_test_dir, file_name), mustWork = FALSE)

    # We do not check whether the file is readable
    # or not because we (sometimes) need to create
    # non-readable mock files for testing purposes.
    if (!utils::file_test("-f", path)) {
        stopf(
            "[TestingError] mock file '%s' does not exist or is a directory.",
            path)
    }

    return(path)
}
