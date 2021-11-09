#' smosaic
#'
#' Shiny app for creating a Mosaic plot with interactive variable selection.
#'
#' @param data table or data.frame
#' @param xvar character: names of selected variables for x-axis
#' @param yvar character: names of selected variables for y-axis
#' @param ... further parameters given to [graphics::mosaicplot()]
#'
#' @md
#' @return nothing
#' @import shiny
#' @import shinydashboard
#' @import sortable
#' @importFrom graphics mosaicplot
#' @export
#'
#' @examples
#' if (interactive()) smosaic(Titanic)
#' dfTitanic <- table2dataframe(Titanic)
#' if (interactive()) smosaic(dfTitanic)
smosaic <- function(data, xvar=character(0), yvar=character(0), ...) {
  main <- paste(deparse(substitute(data), 500), collapse = "\n")
  obj  <- c("matrix", "data.frame", "table") %in% class(data)
  stopifnot(any(obj))
  totab <- main
  if (!obj[3]) { # no table
    if (!obj[2]) { # no data frame
      totab <- sprintf("as.data.frame(%s)", totab)
      data <- as.data.frame(data)  # matrix -> data.frame
    }      
    totab <- sprintf("table(%s)", totab)
    data  <- table(data)
  }
  if (is.null(dimnames(data))) dimnames(data) <- sapply(dim(data), seq)
  if (is.null(names(dimnames(data)))) names(dimnames(data)) <- sprintf("%s[,%.0f]", main, seq(length(dim(data))))
  dvar <- names(dimnames(data))
  stopifnot(length(dvar)>1)  # not enough variables found
  ivar <- intersect(xvar, yvar)
  xvar <- setdiff(xvar, ivar)
  yvar <- setdiff(yvar, ivar)
  xvar <- xvar[xvar %in% dvar]
  if (length(xvar)==0) xvar <- setdiff(dvar, yvar)[1]
  yvar <- yvar[yvar %in% dvar]
  if (length(yvar)==0) yvar <- setdiff(dvar, xvar)[1]
  dvar <- setdiff(dvar, c(xvar, yvar))
  #
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(title="Mosaicplot"),
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
            text = "X",
            labels = xvar,
            input_id = "xvar"
          ),
          add_rank_list(
            text = "Y",
            labels = yvar,
            input_id = "yvar"
          )
        )
      ),
      dashboardBody(
        fluidRow(
          box(plotOutput("mosaic")),
          box(verbatimTextOutput("command"), title="Basic R code")
      ))
    ),
    server = function(input, output, session) {
      output$mosaic <- renderPlot({
        if ((length(input$xvar)>0) && (length(input$yvar)>0)) {
          #browser()
          args     <- list(...)
          args$x   <- apply(data, c(input$xvar, input$yvar), sum)
          args$dir <-  c(rep("v", length(input$xvar)), rep("h", length(input$yvar)))
          if (is.null(args$main)) args$main <- main
          do.call("mosaicplot", args)
        }
      })

      output$command <- renderText({
        txt <- "At least two variables are required for a plot!"
        if ((length(input$xvar)>0) && (length(input$yvar)>0)) {
          txt <- c(paste0(" tab <- ", totab, "\n"),
                   paste0("x   <- c(", paste0('"', input$xvar, '"', collapse=", "), ")\n"),
                   paste0("y   <- c(", paste0('"', input$yvar, '"', collapse=", "), ")\n"),
                   "tab <- apply(tab, c(x, y), sum)\n",
                   "dir <- c(rep(\"v\", length(x)), rep(\"h\", length(y)))\n",
                   "mosaicplot(tab, dir=dir)\n")
        }
        txt
      })
    }
  )
}
