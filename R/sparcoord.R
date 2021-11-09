#' sandrews
#'
#' Shiny app for creating a Parallel Coordinate plot with interactive variable selection.
#'
#' @param data matrix or data.frame
#' @param xvar character: names of selected variables for the plot
#' @param ... further parameters given to [MASS::parcoord()]
#'
#' @md
#' @return nothing
#' @import shiny
#' @import shinydashboard
#' @import sortable
#' @importFrom MASS parcoord
#' @export
#'
#' @examples
#' if (interactive()) sparcoord(iris, col=as.factor(iris$Species))
sparcoord <- function(data, xvar=character(0), ...) {
  main         <- paste(deparse(substitute(data), 500), collapse = "\n")
  if (is.data.frame(data)) data <- as.matrix(data[,sapply(data, is.numeric)])
  stopifnot("matrix" %in% class(data))
  if (is.null(colnames(data))) colnames(data) <- sprintf("%s[,%.0f]", main, 1:ncol(data))
  dvar <- dimnames(data)[[2]]
  if (length(xvar)) {
    xvar <- xvar[xvar %in% dvar]
    dvar <- setdiff(dvar, xvar)
  } else {
    xvar <- dvar
    dvar <- NULL
  }
  #
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(title="Parallel coordinate plot"),
      dashboardSidebar(
        tags$style( HTML(".black-text .rank-list-item { color: #000000; }")),
        bucket_list(
          header = NULL,
          group_name = "bucket_var_group",
          orientation = "vertical",
          class = c("default-sortable", "black-text"),
          add_rank_list(
            text = "Variable(s)",
            labels = dvar,
            input_id = "dvar"
          ),
          add_rank_list(
            text = "Selected variable(s)",
            labels = xvar,
            input_id = "xvar"
          )
        )
      ),
      dashboardBody(
        fluidRow(
          box(plotOutput("plot")),
          box(verbatimTextOutput("command"), title="Basic R code")
      ))
    ),
    server = function(input, output, session) {
      output$plot <- renderPlot({
        if ((length(input$xvar)>1)) {
          #browser()
          args     <- list(...)
          args$x   <- data[,input$xvar]
          if (is.null(args$main)) args$main <- main
          do.call("parcoord", args)
        }
      })

      output$command <- renderText({
        txt <- "At least two variables are required for a plot!"
        if (length(input$xvar)>1) {
          txt <- c(paste0(" x <- c(", paste0('"', input$xvar, '"', collapse=", "), ")\n"),
                   sprintf("parcoord(%s[,x])\n", main))
        }
        txt
      })
    }
  )
}
