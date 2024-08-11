#' @export
my_rcoal2 = function (n=20, curr.popsize=1000,old.popsize=1,when=10)
{
  tip.label = NULL
  nbr <- 2 * n - 2
  edge <- matrix(NA, nbr, 2)
  x=0
  for (i in seq(n,2,-1)){
    x[(n+1)-i]=-2*curr.popsize*log(1-runif(1))/(i*(i-1))
  }

  rescale=which(cumsum(x)>when)
  x[rescale]=x[rescale]*old.popsize/curr.popsize


  edge.length <- numeric(nbr)
  h <- numeric(2 * n - 1)
  node.height <- cumsum(x)
  pool <- 1:n
  nextnode <- 2L * n - 1L
  for (i in 1:(n - 1)) {
    y <- sample(pool, size = 2)
    ind <- (i - 1) * 2 + 1:2
    edge[ind, 2] <- y
    edge[ind, 1] <- nextnode
    edge.length[ind] <- node.height[i] - h[y]
    h[nextnode] <- node.height[i]
    pool <- c(pool[!pool %in% y], nextnode)
    nextnode <- nextnode - 1L
  }

  phy <- list(edge = edge, edge.length = edge.length)
  if (is.null(tip.label))
    tip.label <- paste("t", 1:n, sep = "")
  phy$tip.label <- sample(tip.label)
  phy$Nnode <- n - 1L
  class(phy) <- "phylo"
  phy <- reorder(phy)
  phy$edge[phy$edge[, 2] <= n, 2] <- 1:n
  phy
}
