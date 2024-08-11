#' @export
Fig9_4and5=function (popsize=100)
{
  library(ape)
  tree=my.rcoal2(n=20,popsize,popsize,0)
  depth = get.tree.depth(tree)
  plot(c(0,20,0,20),c(depth,0,depth,0),type='n',axes=F,xlab='',ylab='')
  plot.tree(tree,muts=F)
  axis(2)
}
