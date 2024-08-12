#' @title Probability of getting observed AA, AB, and BB genotype
#' frequencies if locus is in Hardy-Weinberg equilibrium
#' @description As in the title.
#' @details The object is called Table3_2 as it is Table 3.2 on page 59 of Mielke, Konigsberg,
#' and Relethford (2011) and Table 3.3 page 56 of the 2006 edition.  The defaults values are
#' from Mitchell et al. (1999).
#' @param AA Integer giving count of AA genotype. Default=136
#' @param AB Integer giving count of AB genotype. Default=66
#' @param BB Integer giving count of BB genotype. Default=17
#' @param stop Ending number of alleles. Default=50
#' @return Returns the observed genotype counts, the expected values if the locus
#' was in Hardy-Weinberg, and the probability value of getting the observed counts
#' if the locus was in Hardy-Weinberg equilibrium.  It also shows graphically how
#' the probability value was obtained.
#' @references
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2011)
#' \emph{Human biological variation}, 2nd ed.  New York: Oxford University Press.
#'
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2006)
#' \emph{Human biological variation}, 1st ed.  New York: Oxford University Press.
#'
#' Mitchell, R., Howlett, S., White, N., Federle, L., Papiha, S. S., Briceno, I.,
#'  . . . Osipova, L. (1999). Deletion polymorphism in the human COL1A2 gene:
#'  genetic evidence of a non-African population whose descendants spread to
#'  all continents. \emph{Human Biology}, 901-914.
#' @export
Tab3_2=function (AA=136,AB=66,BB=17)
  {
    AA=round(AA,0)
    AB=round(AB,0)
    BB=round(BB,0)
    options(warn=-1)
    N=AA+AB+BB
    Two.N=2*N
    A=2*AA+AB
    B=2*BB+AB
    f.A=(2*AA+AB)/Two.N
    f.B=1-f.A
    cat('\nObserved values:\n')
    cat(c(AA,AB,BB))
    cat('\n')
    cat('Expected values:\n')
    cat(c(f.A^2*N,2*f.A*f.B*N,f.B^2*N))
    cat('\n')
    p = function(AA,AB,BB) return(exp(lgamma(N+1)+lgamma(A+1)+lgamma(B+1)+log(2)*AB
                                      -lgamma(AA+1)-lgamma(AB+1)-lgamma(BB+1)-lgamma(Two.N+1)))
    start.AA=(A-B)/2
    stop.AA=A/2
    sto.p=0
    obs.p=p(AA,AB,BB)
    for(i in start.AA:stop.AA){
      t.AA=i
      t.AB=A-2*t.AA
      t.BB=N-t.AA-t.AB
      sto.p[i-start.AA+1]=p(t.AA,t.AB,t.BB)
    }
    stop.AA=(start.AA:stop.AA)[max(which(sto.p>1.e-8))]
    top=max(sto.p)*1.1
    plot(c(start.AA-.5,stop.AA+0.5),c(0,top),type='n',xlab='Number of AA genotypes',
         ylab='Probability')
    tot.p=0
    for(i in start.AA:stop.AA){
      curr=i-start.AA+1
      if(sto.p[curr]<=obs.p) {
        tot.p=tot.p+sto.p[curr]
        rect(i-.5,0,i+.5,sto.p[curr],density=40)
      }
      else rect(i-.5,0,i+.5,sto.p[curr])
    }
    lines(c(AA,AA),c(0,1))
    cat('p-value:\n')
    cat(round(tot.p,6))
    cat('\n')
  }
