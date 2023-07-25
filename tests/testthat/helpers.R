#' Add simple notes to snapshots
#'
#' Annotate outputs recorded by [testthat::expect_snapshot()] and other related
#' functions.
#'
#' @param note a character string. The *note* to record with an output. It is
#'   intended to be (really) small.
#'
#' @returns
#' Nothing. [addNote()] is used for its side-effect.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @family helper functions for testing
#'
#' @keywords internal
addNote <- function(note = character(1L)) {
    cat("[ NOTE ] ", note, "\n", sep = "")
    return(invisible())
}
