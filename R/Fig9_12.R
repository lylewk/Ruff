#' @title Simulation of coalescent tree with mutation and population expansion
#' @description Plots both the coalescent tree with mutations marked as dots and
#' the mismatch distribution as a histogram.  Unlike Fig9_7(), this one includes
#' population expansion
#' @details
#' The object is called Fig9_12 as it is Figure 9.12 and 9.13 on page 218 of Mielke, Konigsberg,
#' and Relethford (2011) and on page 208 of the 2006 edition where it is Figure 8.12
#' and 8.13.It plots a coalescent tree with 36 "tips" (current alleles). And places mutations
#' (as dots) assuming a constant mutation rate.  Because of the constant rate, you
#' expect more mutations (dots) along longer (deeper) branches and less mutations
#' along shorter (shallower) branches.  This object also produces a histogram of
#' mismatches (the mismatch distribution).  The mismatch between two current alleles
#' is the number of "dots" from counting up from one "tip" to the most recent common ancestor
#' and then counting down to the other "tip."  Because of the population expansion
#' in the past, the simulated tree has a comb-like appearance without any deep routes.
#' Consequently, the mismatch distribution, instead of being ragged, will appear smooth
#' (wave-like).
#' @param old.popsize An integer giving the founding population size. Default=2.
#' @param when A number giving the the "mutational units" ago at which expansion
#' occured.
#' @return Plot of a coalescent tree with mutations as dots and of the resulting
#' mismatch distribution.  Note that you may have to hunt around for the mismatch
#' distribution.  If you are using R (instead of RStudio) you may have to drag one
#' of the plots to see the other.  If you are using RStudio you may have an "unseen"
#' mismatch distribution plot appearing as an icon that you will have to open.
#' @examples
#' \dontrun{
#' # Default with population expansion
#' # Note: because of randomness, you may have to run
#' #       multiple times to get smooth mismatch and
#' #       comb-like coalescent tree
#' Fig9_12()
#' # No population expansion
#' Fig9_12(1000,0)
#' }
#' @references
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2011)
#' \emph{Human biological variation}, 2nd ed.  New York: Oxford University Press.
#'
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2006)
#' \emph{Human biological variation}, 1st ed.  New York: Oxford University Press.
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
