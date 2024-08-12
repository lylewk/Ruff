#' @title Temporal change in allele frequency given relative fitness values
#' @description Shows effect of various selection regimes in a biallelic system.
#' @details
#' The object is called Fig3_9 as it is Figure 3.9 on page 71 of Mielke, Konigsberg,
#' and Relethford (2011) and on page 67 of the 2006 edition where it is Figure 3.4.
#' @param w Vector of three values giving the relative fitness for each genotype,
#' AA, AB, and BB.  As these are relative fitness values, at least one value must
#' be 1.0. Default = c(0.7635,1,0.0001)
#' @param p.start A vector of starting allele frequencies.  If you use p.start=runif(100),
#' you will get 100 random starting values. Default=c(0.0001,0.9999)
#' @return Plot of allele frequencies across generations.
#' @references
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2011)
#' \emph{Human biological variation}, 2nd ed.  New York: Oxford University Press.
#'
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2006)
#' \emph{Human biological variation}, 1st ed.  New York: Oxford University Press.
#' @export
Fig3_9=function (w=c(0.7635,1,0.0001),p.start=c(0.0001,0.9999))
{
  p=0.0001
  t=0:42
  w.AA=w[1]
  w.AS=w[2]
  w.SS=w[3]

  p[1]=p.start[1]
  for(i in 2:43){
    ave.w=p[i-1]^2*w[1]+2*p[i-1]*(1-p[i-1])*w[2]+(1-p[i-1])^2*w[3]
    p[i]=(p[i-1]^2*w[1]+p[i-1]*(1-p[i-1])*w[2])/ave.w
  }
  plot(t,p,type='l',xlim=c(0,40),ylim=c(0,1),xlab='Generation',
       ylab='Allele Frequency (A)')

  n.starts=length(p.start)
  for(j in 2:n.starts){
    p[1]=p.start[j]
    for(i in 2:43){
      ave.w=p[i-1]^2*w[1]+2*p[i-1]*(1-p[i-1])*w[2]+(1-p[i-1])^2*w[3]
      p[i]=(p[i-1]^2*w[1]+p[i-1]*(1-p[i-1])*w[2])/ave.w}
    lines(t,p)
  }
}
