#' @title EM algorithm to estimate allele frequencies in ABO system
#' @description Calculates the allele frequencies in the ABO blood system
#' using the EM algorithm.
#' @details This object takes as input the counts for Type A blood,
#' Type B blood, Type AB blood, and Type O blood and find the allele frequencies
#' for the A allele (=p), the B allele (=q), and the O allele (=r).  From
#' these estimated allele frequencies it also gives the expected phenotype counts
#' using Hardy-Weinberg equilibrium.  There are no parameters as the object prompts
#' for the blood type counts. The suggested blood type counts are from Taylor and
#' Prior (1938).  The object is called "P_56" as page 56 in Mielke, Konigsberg,
#' and Relethford (2011) has an extremely abbreviated example using the Taylor and
#' Prior (1938) data.  Pages 52-53 (and Table 3.2) in the first edition (2006)
#' of the text give a more detailed treatment of the EM algorithm.
#' @return Returns the allele frequencies and the expected phenotype counts under
#' Hardy-Weinberg equilibrium.
#' @references
#' Taylor, G. L., & Prior, A. M. (1938). Blood groups in Britain II:
#' Distribution in the populations. \emph{Annals of Eugenics}, 8, 356-361.
#'
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2011)
#' \emph{Human biological variation}, 2nd ed.  New York: Oxford University Press.
#'
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2006)
#' \emph{Human biological variation}, 1st ed.  New York: Oxford University Press.
#' @export
P_56=function(){
  cat('Enter the number of individuals with type-A blood (179 in Taylor & Prior)', fill=F ); A = scan(n=1,quiet=T)
  cat('Enter the number of individuals with type-B blood (35 in T & P)', fill=F ); B = scan(n=1,quiet=T)
  cat('Enter the number of individuals with type-AB blood (6 in T & P)', fill=F ); AB = scan(n=1,quiet=T)
  cat('Enter the number of individuals with type-O blood (202 in T & P)', fill=F ); O = scan(n=1,quiet=T)

  p=0;q=0;r=0
  N=A+B+AB+O
  r[1]=sqrt(O/N)
  p[1]=(1-r[1])/2
  q[1]=p[1]
  for(i in 2:10){
    AA=p[i-1]^2/(p[i-1]^2+2*p[i-1]*r[i-1])*A
    AO=A-AA
    BB=q[i-1]^2/(q[i-1]^2+2*q[i-1]*r[i-1])*B
    BO=B-BB
    p[i]=(2*AA+AO+AB)/(2*N)
    q[i]=(2*BB+BO+AB)/(2*N)
    r[i]=1-p[i]-q[i]
  }
  ABO = matrix(c(A,(p[10]^2+2*p[10]*r[10])*N,B,(q[10]^2+2*q[10]*r[10])*N,
                 AB,(2*p[10]*q[10])*N,O,r[10]^2*N),nc=2,byrow=T)
  colnames(ABO)=c('Obs','Exp')
  rownames(ABO)=c('A','B','AB','O')
  cat('\n')
  print(ABO)
  cat('\n')
  return(list(N=N,p=p[10],q=q[10],r=r[10]))
}
