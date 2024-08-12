#' @title Stochastic simulation of genetic drift
#' @description Shows plot of sub-population allele frequencies against generations
#' where individual allele frequencies are subject to drift.
#' @details
#' The object is called Fig3_17 as it is Figure 3.17 on page 77 of Mielke, Konigsberg,
#' and Relethford (2011) and on page 73 of the 2006 edition where it is Figure 3.12.
#' The plot draws stochastic "trip" of sub-population allele frequencies across generations.
#' If the allele frequency reaches fixation (equal to 0.0 for the A allele or 1.0 for
#' the B alleles) then the state of fixation has been reached and the allele frequency
#' cannot change.
#' @param N An integer giving the population size for each sub-population.  Note that
#' the number of allele is 2xN, which is always an even number. The allele frequency for
#' all subpopulations starts at p=0.5 (N/2N). Default = 10
#' @param gens An integer giving the number of generations to be followed. Default=100
#' @param y A vector containing two elements that give the limits for the relative
#' fitness values for the plot.  This is used to "zoom in" on the graph.  Default=c(0.77,0.82)
#' @param jit A random "jitter" to be applied when the sub-population size is small.
#' When the sub-population sizes are small it can be difficult to disttinguish lines.
#' The "jitter" gets around this problem.  Be sure not to set the jitter too high.  Values
#' not above 0.01 should be sufficient.  Default=0.0
#' @return Plot of allele frequencies against generations withinh each sub-population.
#' @references
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2011)
#' \emph{Human biological variation}, 2nd ed.  New York: Oxford University Press.
#'
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2006)
#' \emph{Human biological variation}, 1st ed.  New York: Oxford University Press.
#' @export
Fig3_17=function (N=10,gens=100,jit=0)
{
  demes=100
  sim.line=function(){
    p=0
    p[1]=0.5
    for(i in 2:gens){
      p[i]=rbinom(1,2*N,p[i-1])/(2*N)
    }
    return(p)
  }

  p=sim.line()
  plot(1:gens,jitter(p,jit),type='l',xlab='Generation',ylab='p',ylim=c(0,1),main=paste('N = ',N))
  for(i in 2:demes){
    p=sim.line()
    lines(1:gens,jitter(p,jit))
  }
}
