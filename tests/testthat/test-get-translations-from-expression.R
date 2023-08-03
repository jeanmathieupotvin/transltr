# Setup ------------------------------------------------------------------------


# A mock expression vector as base::parse() could
# produce when parsing the contents of a script.
mockExpression <- expression(
    transltr::translate(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
        "Suspendisse et tincidunt nisl. Quisque vel malesuada erat.",
        "Nulla nec suscipit libero id molestie massa."),
    {
        x    <- faithful$waiting
        bins <- seq(min(x), max(x), length.out = 10L)

        hist(x,
            breaks = bins,
            col    = "blue",
            border = "white",
            xlab   = translate("Waiting time to next eruption", "(in mins)"),
            ylab   = translate("Frequency"),
            main   = translate("Histogram of waiting times", lang = "en"))
    }
)

# There are 4 calls to translate() in mockExpression.
attr(mockExpression, "nTranslateCalls") <- 4L


# Tests ------------------------------------------------------------------------


test_that("getTranslationsFromExpression() returns a list", {
    expect_type(getTranslationsFromExpression(), "list")
})

test_that("getTranslationsFromExpression() returns a sorted list (by signatures)", {
    expr    <- expression(translate("a"), transltr::translate("b"))
    strings <- getTranslationsFromExpression(expr)

    expect_length(strings, 2L)
    expect_false(is.unsorted(names(strings)))  ## sorted list
})

test_that("getTranslationsFromExpression() extracts strings properly", {
    strings <- getTranslationsFromExpression(mockExpression)

    expect_type(strings, "list")
    expect_length(strings, attr(mockExpression, "nTranslateCalls"))
    expect_snapshot(getTranslationsFromExpression(mockExpression))
})
