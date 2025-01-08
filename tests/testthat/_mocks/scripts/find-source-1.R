#' A mock Shiny application
#'
#' A mock Shiny application used to test the behavior of [find_source()].
#'
#' @usage
#' ## Expected usage in tests/testthat/test-find-source.R
#' text_read(get_mock_path("scripts/find-source-1"))
#'
#' @seealso [Source code](https://github.com/rstudio/shiny-examples/tree/main/001-hello)
NULL


library(shiny)
library(transltr)


# User Interface ---------------------------------------------------------------


ui <- shiny::fluidPage(
    shiny::titlePanel(transltr::translate("Hello Shiny!")),
    # Sidebar layout with input and output definitions.
    shiny::sidebarLayout(
        # Sidebar panel for inputs.
        shiny::sidebarPanel(
            # Input: Slider for the number of bins.
            shiny::sliderInput(
                inputId = "bins",
                label   = transltr::translate("Number of bins:"),
                min     = 1L,
                max     = 50L,
                value   = 30L)
        ),
        # Main panel for displaying outputs.
        shiny::mainPanel(
            # Output: histogram.
            shiny::plotOutput(outputId = "distPlot")
        )
    )
)


# Server -----------------------------------------------------------------------


server <- function(input, output) {
    # Histogram of the Old Faithful Geyser Data.
    # This expression that generates a histogram is wrapped
    # in a call to shiny::renderPlot() to indicate that:
    #   1. it is "reactive" and therefore should be automatically
    #      re-executed when inputs (input$bins) change, and
    #   2. its output type is a plot.
    output$distPlot <- shiny::renderPlot({
        x    <- faithful$waiting
        bins <- seq(min(x), max(x), length.out = input$bins + 1L)

        graphics::hist(x,
            breaks = bins,
            col    = "#75AADB",
            border = "white",
            xlab   = translate("Waiting time to next eruption (in mins)"),
            main   = translate("Histogram of waiting times"))
    })
}


# Create the application and run it --------------------------------------------


shiny::shinyApp(ui = ui, server = server)
