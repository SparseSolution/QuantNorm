# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

quantileNorm <- function(df, group, vals){
  grps <- levels(as.factor(df[,group]))
  r <- length(grps)
  n <- length(df[df[,group] == grps[1], vals])
  normv <- replicate(r, rep(0, n))

  for (i in 1:r){
    ii <- order(df[df[,group] == grps[i], vals])
    normv[,i] <- df[df[,group] == grps[i],][ii, vals]
  }
  normm <- rowMeans(normv)
  for (i in 1:r){
    ii <- order(df[df[,group] == grps[i], vals])
    df[df[,group] == grps[i],][ii, vals] <- normm
  }
  df[,vals]
}

quantileNormMatrix <- function(df){
  ordrd <- t(apply(df, 1, function(x){x[order(x)]}))
  clmns <- colMeans(ordrd)
  t(apply(df, 1, function(x){clmns[order(x)]}))
}

scaleNorm <- function(df, group, vals){
  grps <- levels(as.factor(df[,group]))
  r <- length(grps)
  n <- length(df[df[,group] == grps[1], vals])
  normv <- replicate(r, rep(0, n))

  for (i in 1:r){
    df[df[,group] == grps[i], vals] <- scale(df[df[,group] == grps[i], vals])
  }

  df[,vals]
}
