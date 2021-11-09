#' schernoff
#'
#' Shiny app for creating a Chernoff faces plot with interactive variable selection.
#'
#' @param data matrix or data.frame
#' @param xvar character: names of selected variables for the plot
#' @param ... further parameters given to [DescTools::PlotFaces()]
#'
#' @md
#' @return nothing
#' @import shiny
#' @import shinydashboard
#' @import sortable
#' @importFrom DescTools PlotFaces
#' @export
#'
#' @examples
#' if (interactive()) schernoff(normalize(iris))
schernoff <- function(data, xvar=character(0), ...) {
  main         <- paste(deparse(substitute(data), 500), collapse = "\n")
  if (is.data.frame(data)) data <- as.matrix(data[,sapply(data, is.numeric)])
  stopifnot("matrix" %in% class(data))
  if (is.null(colnames(data))) colnames(data) <- sprintf("%s[,%.0f]", main, 1:ncol(data))
  dvar <- dimnames(data)[[2]]
  if (length(xvar)) {
    xvar <- xvar[xvar %in% dvar]
    dvar <- setdiff(dvar, xvar)
  } else {
    xvar <- order_andrews(data)
    dvar <- NULL
  }
  choices <- as.list(1:10)
  names(choices) <- sprintf("%.0fx%.0f", 1:10, 1:10)
  maxmin <- rbind(apply(data, 2, max), apply(data, 2, min))
  hc     <- hclust(dist(data), method="ward.D2")
  #
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  #
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(title="Chernoff faces"),
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
          box(selectInput("size", "Size", choices=choices, selected=3),
#              sliderInput("n", "Observations per chart", value=1, min=1, max=25, step=1),
              sliderInput("page", "Page", value=1, min=1, max=ceiling(nrow(data)/9), step=1)
          ),
          box(verbatimTextOutput("command"), title="Basic R code")
      ))
    ),
    server = function(input, output, session) {
      rv <- reactiveValues(size=c(3,3), n=1)

      observeEvent (input$size, {
        page <- as.numeric(isolate(input$page))
        size <- as.numeric(input$size)
        n    <- 1 # as.numeric(isolate(input$n))
        max  <- ceiling(nrow(data)/(n*size*size))
        if (page>max) page <- max
        updateSliderInput(session, "page", value = page, max = max)
        rv$size <- c(size, size)
      })

      observeEvent (input$n, {
        page <- as.numeric(isolate(input$page))
        size <- as.numeric(isolate(input$size))
        n    <- 1 # as.numeric(input$n)
        max  <- ceiling(nrow(data)/(n*size*size))
        if (page>max) page <- max
        updateSliderInput(session, "page", value = page, max = max)
        rv$n <- n
      })

      output$plot <- renderPlot({
        if ((length(input$xvar)>1)) {
          par(mfrow=rv$size, mar=c(0,0,1,0))
          first    <- (as.numeric(input$page)-1)*prod(rv$size)+1
          last     <- min(first+prod(rv$size)-1, nrow(data))
          args     <- list(...)
          args$xy  <- data[hc$order[first:last],input$xvar]
          args$scale  <- TRUE
          args$labels <- as.character(hc$order[first:last])
          args$nr     <- rv$size[1]
          args$nc     <- rv$size[2]
          if (is.null(args$main)) args$main <- main
          do.call("PlotFaces", args)
        }
      })

      output$command <- renderText({
        txt <- "At least two variables are required for a plot!"
        if (length(input$xvar)>2) {
          txt <- c(" # variable to face parts (recycled)\n",
                   "#  1=height of face, 2=width of face, 3=shape of face\n",
                   "#  4=height of mouth, 5=width of mouth, 6=curve of smile\n",
                   "#  7=height of eyes, 8=width of eyes, 9=height of hair\n",
                   "# 10=width of hair, 11=styling of hair, 12=height of nose\n",
                   "# 13=width of nose, 14=width of ears, 15=height of ears\n",
                   paste0("x <- c(", paste0('"', input$xvar, '"', collapse=", "), ")\n"),
                   sprintf("PlotFaces(%s[,x])\n", main))
        }
        txt
      })
    }
  )
}
