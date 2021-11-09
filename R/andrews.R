#' andrews
#'
#' Andrews curves for visualization of multidimensional data.
#' \code{step} determines the number of line segments for each curve.
#' If \code{ymax==NA} then the maximum y coordinate will be determined from the curves.
#' Note that for \code{type==3} the x range is \eqn{[0, 4*pi]} otherwise \eqn{[-pi, pi]}.
#'
#' @param x data frame or matrix
#' @param type type of curve (default: \code{1})
#' * 1: \eqn{f(t)=x1/(2^0.5)+x2*sin(t)+x3*cos(t)+x4*sin(2*t)+x5*cos(2*t)+...}
#' * 2: \eqn{f(t)=x1*sin(t)+x2*cos(t)+x3*sin(2*t)+x4*cos(2*t)+...}
#' * 3: \eqn{f(t)=x1*cos(t)+x2*cos((2*t)^0.5)+x3*cos((3*t)^0.5)+...}
#' * 4: \eqn{f(t)=1/(2^0.5)*(x1+x2*(sin(t)+cos(t))+x3*(sin(t)-cos(t))+x4*(sin(2*t)+cos(2*t))+x5*(sin(2*t)-cos(2*t))+...)}
#' @param step smoothness of curves
#' @param ... further parameters given to [graphics::plot] and [graphics::lines()]
#' @param normalize integer: normalization method (default: \code{1})
#' * 0: no rescaling
#' * 1: \eqn{(x-min(x))/(max(x)-min(x))}
#' * 2: \eqn{(x-mean(x))/sd(x)}
#' @param ymax numeric: maximum of y coordinate (default: \code{NA})
#' @seealso In package \code{\link[andrews]{andrews}} or at \href{https://CRAN.R-project.org/package=andrews}{CRAN}
#' @references
#' * Andrews, D. F. (1972) Plots of High-Dimensional Data. Biometrics, vol. 28, no. 1, pp. 125-136.
#' * Khattree, R., Naik, D. N. (2002) Andrews Plots for Multivariate Data: Some New Suggestions and Applications. Journal of Statistical Planning and Inference, vol. 100, no. 2, pp. 411-425.
#'
#' @importFrom graphics lines
#' @return nothing
#' @md
#' @export
#'
#' @examples
#' andrews(iris[,-5], col=as.factor(iris[,5]))
#' andrews(iris[,-5], type=4, col=as.factor(iris[,5]), ymax=2)
andrews <- function(x, type=1, step=100, ..., normalize=1, ymax=NA) {
  curvetype <- function(type, i, t) {
    M_SQRT1_2 <- 1/sqrt(2)
    if (type==1) {
      if(i==1) {
        #print("1/sqrt(2)")
        return(rep(M_SQRT1_2, length(t)))
      }
      k <- (i - 2)%/%2 + 1
      if (i%%2) {
        #print(sprintf("cos(%.0ft)", k))
        return(cos(k*t))
      }
      #print(sprintf("sin(%.0ft)", k))
      return(sin(k*t))
    }
    if (type==2) {
      k <- (i+1)%/%2
      if ((i+1)%%2) {
        #print(sprintf("cos(%.0ft)",k ))
        return(cos(k*t))
      }
      #print(sprintf("sin(%.0ft)", k))
      return(sin(k*t))
    }
    if (type==3) {
      if (i==1) {
        #print("cos(t)")
        return(cos(t))
      }
      #print(sprintf("cos((%.0ft)^0.5)", i))
      return(cos(sqrt(i*t)))
    }
    if (type==4) {
      if (i==1) {
        #print("1/sqrt(2)")
        return(rep(M_SQRT1_2, length(t)))
      }
      k <- i%/%2
      if ((i+1)%%2) {
        #print(sprintf("sin(%.0ft)+cos(%.0ft)", k, k))
        return((sin(k*t)+cos(k*t))*M_SQRT1_2)
      }
      #print(sprintf("sin(%.0ft)-cos(%.0ft)", k, k))
      return((sin(k*t)-cos(k*t))*M_SQRT1_2)
    }
  }
  #
  t  <- if (type==3) seq(0, 4*pi, length.out=step+1) else seq(-pi, pi, length.out=step+1)
  x  <- normalize(x, method=normalize)
  xt <- NULL
  for (j in 1:ncol(x)) xt <- cbind(xt, curvetype(type, j, t))
  if (is.na(ymax)) {
    rg <- 0
    for (i in 1:nrow(x)) rg <- range(rg, xt%*%x[i,])
    mrg <- max(abs(rg))
  } else {
    mrg <- abs(ymax)
  }
  args <- list(...)
  args$x    <- range(t)
  args$y    <- c(-mrg, mrg)
  args$type <- "n"
  if (is.null(args$xlab)) args$xlab <- ""
  if (is.null(args$ylab)) args$ylab <- ""
  do.call(plot, args)
#  args <- list(...)
  for (i in 1:nrow(x)) {
    col    <- if (is.null(args$col[i])) 1 else args$col[i]
    lend   <- if (is.null(args$lend[i])) 0 else args$lend[i]
    ljoin  <- if (is.null(args$ljoin[i])) 0 else args$ljoin[i]
    lmitre <- if (is.null(args$lmitre[i])) 10 else args$lmitre[i]
    lty    <- if (is.null(args$lty[i])) 1 else args$lty[i]
    lwd    <- if (is.null(args$lwd[i])) 1 else args$lwd[i]
    lines(t, xt%*%x[i,], col=col, lend=args$lend, ljoin=ljoin, lmitre=lmitre,
          lty=lty, lwd=lwd)
  }
}
