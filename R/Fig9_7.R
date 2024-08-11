#' @export
Fig9_7 = function ()
{
  library(ape)
  tree=my_rcoal2(n=36,100,100,0)
  depth=get_tree_depth(tree)
  plot(c(0,37,0,37),c(depth,0,depth,0),type='n',axes=F,xlab='',ylab='')
  plot_tree(tree,muts=T)
}
