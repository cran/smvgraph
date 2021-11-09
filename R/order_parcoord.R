#' order_parcoord
#'
#' Returns a reordering of the columns of \code{x} to visualize highly correlated variable pairs
#' based on a cluster analysis of the correlation matrix. 
#' If no colum names are given then \code{V1}, \code{V2}, ... will be used.
#'
#' @param x data matrix
#' @param method numeric: order method (default: \code{"spearman"})
#' @param ... further parameters given to [stats::cor()] 
#'
#' @md
#' @return order of column vectors
#'
#' @importFrom stats cor hclust
#' @export
#'
#' @examples
#' order_parcoord(iris)
order_parcoord <- function(x, method="spearman", ...) {
  x  <- normalize(x, 0)
  c  <- cor(x, method=method, ...)
  hc <- hclust(dist(c^2), method="ward.D2")
  colnames(x)[hc$order]
}
