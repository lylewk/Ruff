#' @export
Fig9_6 = function (popsize1=100,popsize2=1000)
{
  library(ape)
  library(abe)
  tree1=my_rcoal2(n=20,popsize1,popsize1,0)
  depth1 = get_tree_depth(tree1)
  tree2=my_rcoal2(n=20,popsize2,popsize2,0)
  depth2 = get_tree_depth(tree2)
  max.depth=max(c(depth1,depth2))
  plot(c(0,46,0,46),c(max.depth,0,max.depth,0),type='n',axes=F,xlab='',ylab='')
  plot_tree(tree1,muts=F)
  plot_tree(tree2,x.offset=25,muts=F)
  axis(2)
}
