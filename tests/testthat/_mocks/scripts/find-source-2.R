#' A mock R script
#'
#' A mock R script used to test the behavior of [find_source()].
#'
#' @usage
#' ## Expected usage in tests/testthat/test-find-source.R
#' text_read(get_mock_path("scripts/find-source-2"))
NULL

# Typical use cases.
translate("a")
transltr::translate("b")

# Quoted arguments to `::`.
"translate"("c")
"transltr"::translate("d")
transltr::"translate"("e")
"transltr"::"translate"("f")

# Backticks (syntactic names).
`translate`("g")
`transltr`::translate("h")
transltr::`translate`("i")
`transltr`::`translate`("j")

# Mix of quotes and backticks.
"transltr"::`translate`("k")
`transltr`::"translate"("l")

# Embedded calls.
embeddedCall(translate("m"))
embeddedCall(transltr::translate("n"))
