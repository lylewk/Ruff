#' @title Simulation of coalescent tree with mutation
#' @description Plots both the coalescent tree with mutations marked as dots and
#' the mismatch distribution as a histogram.
#' @details
#' The object is called Fig9_7 as it is Figure 9.7 on page 213 of Mielke, Konigsberg,
#' and Relethford (2011) and on page 204 of the 2006 edition where it is Figure 8.7.
#' It plots a coalescent tree with 36 "tips" (current alleles). And places mutations
#' (as dots) assuming a constant mutation rate.  Because of the constant rate, you
#' expect more mutations (dots) along longer (deeper) branches and less mutations
#' along shorter (shallower) branches.  This object also produces a histogram of
#' mismatches (the mismatch distribution).  The mismatch between two current alleles
#' is the number of "dots" from counting up from one "tip" to the most recent common ancestor
#' and then counting down to the other "tip."  Because the rate of coalesence decreases
#' with the fewer distinct lines as you move "up" the tree, there will be a few deep
#' branches.  This gives the mismatch distribution a ragged appearance.  There are
#' no parameters for this script as everything is "hard-wired."
#' @return Plot of a coalescent tree with mutations as dots and of the resulting
#' mismatch distribution.  Note that you may have to hunt around for the mismatch
#' distribution.  If you are using R (instead of RStudio) you may have to drag one
#' of the plots to see the other.  If you are using RStudio you may have an "unseen"
#' mismatch distribution plot appearing as an icon that you will have to open.
#' @references
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2011)
#' \emph{Human biological variation}, 2nd ed.  New York: Oxford University Press.
#'
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2006)
#' \emph{Human biological variation}, 1st ed.  New York: Oxford University Press.
#' @export
Fig9_7 = function ()
{
  library(ape)
  tree=my_rcoal2(n=36,100,100,0)
  depth=get_tree_depth(tree)
  plot(c(0,37,0,37),c(depth,0,depth,0),type='n',axes=F,xlab='',ylab='')
  plot_tree(tree,muts=T)
}
