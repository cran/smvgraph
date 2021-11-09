if (FALSE) {
  library("smvgraph")
  sandrews(iris)
  sandrews(normalize(iris, 0))
  schernoff(iris)
  schernoff(normalize(iris, 0))
  spairs(iris)
  spairs(normalize(iris, 0))
  sparcoord(iris)
  sparcoord(normalize(iris, 0))
  sradar(iris)
  sradar(normalize(iris, 0))
  m <- matrix(runif(36), ncol=4)
  sandrews(m)
  schernoff(m)
  spairs(m)
  sparcoord(m)
  sradar(m)
  #
  smosaic(Titanic)
  smosaic(table2dataframe(Titanic))
  m <- matrix(sample(1:6, size=36, replace = TRUE), ncol=4)
  smosaic(m)
  t <- Titanic
  dimnames(t) <- NULL
  smosaic(t)
}