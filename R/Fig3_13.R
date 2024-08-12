#' @title Plot of average relative fitness against allele frequencies.
#' @description Shows the curve of average relative fitness against allele frequencies
#' in a biallelic system under selection regimes.
#' @details
#' The object is called Fig3_13 as it is Figure 3.13 on page 74 of Mielke, Konigsberg,
#' and Relethford (2011) and on page 70 of the 2006 edition where it is Figure 3.8.
#' @param w Vector of three values giving the relative fitness for each genotype,
#' AA, AB, and BB.  As these are relative fitness values, at least one value must
#' be 1.0. Default = c(0.7635,1,0.0001)
#' @param x A vector containing two elements that give the limits for the allele
#' frequencies for the plot.  This is used to "zoom in" on the graph.  Default=c(0.65,0.95)
#' @param y A vector containing two elements that give the limits for the relative
#' fitness values for the plot.  This is used to "zoom in" on the graph.  Default=c(0.77,0.82)
#' @param tan.int A real value giving the frequency of the tangents to the curve.
#' The constraint is that 0<= tan.int <=1. Values closer to 0.0 give more tangents
#' and  values closer to 1 give fewer tangents. Default=0.01
#' @return Plot of average relative fitness against allele frequencies with tangents
#' drawn to the curve.
#' @references
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2011)
#' \emph{Human biological variation}, 2nd ed.  New York: Oxford University Press.
#'
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2006)
#' \emph{Human biological variation}, 1st ed.  New York: Oxford University Press.
#' @export
Fig3_13=function (w=c(0.7635,1,0.0001),x=c(0.65,0.95),y=c(0.77,0.82),
                  tan.int=.01)
{
  w.AA=w[1]
  w.AB=w[2]
  w.BB=w[3]

  p=seq(0.01,0.99,0.01)
  ave.w = function(p) return(p^2*w[1]+2*p*(1-p)*w[2]+(1-p)^2*w[3])
  slope.w=function(p){
    return(2*(p*(w.AA-w.AB)+(1-p)*(w.AB-w.BB)))
  }
  plot(p,ave.w(p),type='l',xlim=x,ylim=y)
  tangent=function(p){
    b=slope.w(p)
    a=ave.w(p)-p*b
    abline(a,b)
  }
  tans=seq(x[1],x[2],tan.int)
  n=length(tans)
  for(i in 1:n) tangent(tans[i])
}
