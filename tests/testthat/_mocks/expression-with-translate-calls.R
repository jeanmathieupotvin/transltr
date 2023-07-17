# A mock expression vector as base::parse() could
# produce when parsing the contents of a script.
expression(
    transltr::translate(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. ",
        "Suspendisse et tincidunt nisl. Quisque vel malesuada erat.",
        "Nulla nec suscipit libero id molestie massa."),
    {
        x    <- faithful$waiting
        bins <- seq(min(x), max(x), length.out = 10L)

        hist(x,
            breaks = bins,
            col    = "blue",
            border = "white",
            xlab   = translate("Waiting time to next eruption ", "(in mins)"),
            ylab   = translate("Frequency"),
            main   = translate("Histogram of waiting times", lang = "en"))
    }
)
