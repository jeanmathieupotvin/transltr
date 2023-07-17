#' Declare dependencies
#'
#' Add dependencies to `DESCRIPTION`. Packages `covr` and `testthat` were
#' added at inception because they are required to execute unit tests.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @seealso [The usethis package](https://usethis.r-lib.org/)
NULL


usethis::use_package("R", "Depends", min_version ="4.0")
usethis::use_package("sodium", "Imports")
