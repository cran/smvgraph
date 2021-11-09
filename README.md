# smvgraph

Mosaic diagram, scatterplot matrix, Andrews curves, parallel coordinate diagram, radar diagram, and Chernoff plots  as a Shiny app, which allow the order of variables to be changed interactively. The apps are intended as teaching examples.

```r
library("smvgraph")
smosaic(Titanic)
sandrews(iris)
schernoff(iris)
spairs(iris)
sparcoord(iris)
sradar(iris)
```

# Installation  

## From CRAN

```R
install.packages("smvgraph")
```

## From github

Note that from github you install the current development version.

```R
library("devtools")
install_github("sigbertklinke/smvgraph")
```
