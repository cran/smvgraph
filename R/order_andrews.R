#' order_andrews
#'
#' Returns a reording of the columns of \code{x} to visualize outliers or clusters better.
#' If no colum names are given then \code{V1}, \code{V2}, ... will be used.
#'
#' @param x data matrix
#' @param method numeric: order method (default: \code{1})
#' * 1: interquartile range
#' * 2: \eqn{max(x-median(x))/IQR(x)} (outlier)
#' * 3: fit to a Ward cluster solution with euclidean distance
#'
#' @md
#' @return order of column vectors
#'
#' @importFrom stats IQR cor dist hclust median
#' @export
#'
#' @examples
#' order_andrews(iris)
order_andrews <- function(x, method=1) {
  x <- normalize(x, 0)
  if (method==1) o <- apply(x, 2, IQR, na.rm=TRUE)
  if (method==2) o <- apply(x, 2, function(v) { max((v-median(v, na.rm=TRUE)))/IQR(v, na.rm=TRUE)})
  if (method==3) {
    hc <- hclust(dist(x), method="ward.D2")
    o  <- apply(x, 2, function(v) { abs(cor(order(v), hc$order, method = "spearman")) })
  }
  o <- order(o, decreasing = TRUE)
  colnames(x)[o]
}
