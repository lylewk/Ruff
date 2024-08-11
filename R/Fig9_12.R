#' @export
Fig9_12 = function (old.popsize=2,when=10)
{
  library(ape)
  tree<-my_rcoal2(n=36,1000,old.popsize,when)
  depth=get_tree_depth(tree)
  plot(c(0,37,0,37),c(depth,0,depth,0),type='n',axes=F,xlab='',ylab='',
       main=paste('Old size = ',old.popsize, 'Expansion at ',when))
  mut.rate=(old.popsize-2)*(0.01-.25)/998+.25
  plot_tree(tree)
}
