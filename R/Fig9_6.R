#' @title Plot of two coalescent trees
#' @description Plots simulation of two coalescent trees.
#' @details
#' The object is called Fig9_6 as it is Figure 9.6 on page 212 of Mielke, Konigsberg,
#' and Relethford (2011) and on page 202 of the 2006 edition where it is Figure 8.6.
#' It plots two coalescent trees side by side (on the same scale) each with 20 "tips"
#' (current alleles). The two trees can represent different population sizes. The
#' point of this is to show the relationship between trees based on different population
#' sizes, where coalescent trees are shallower if the population size is smaller
#' and deeper if the population size is larger.  To wit, coalescence is more rapid
#' when the population size is small and slower when the population size is smaller.
#' @param popsize1 Integer value giving population size for first coalescent tree.
#' Default = 100
#' @param popsize2 Integer value giving population size for first coalescent tree.
#' Default = 1000
#' @return Plot of two coalescent trees.
#' @references
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2011)
#' \emph{Human biological variation}, 2nd ed.  New York: Oxford University Press.
#'
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2006)
#' \emph{Human biological variation}, 1st ed.  New York: Oxford University Press.
#' @export
Fig9_6 = function (popsize1=100,popsize2=1000)
{
  library(ape)
  set.seed(1234)
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
