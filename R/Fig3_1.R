#' @title Plot heterozygosity against number of alleles
#' @description Calculates heterozygosity from number of alleles and plots
#' against number of alleles.
#' @details The model assumes that there are equal frequencies of the alleles.
#' For example, if there are five different alleles, each has a frequency of 1/5.
#' Heterozygosity is the frequency of heterozygotes.The equation for heterozygosity
#' is:
#' \deqn{H(i)=2\times \left( \begin{array}{c} i \\ 2 \end{array} \right)\times \left\{\frac{1}{i}\right\}^{2}}
#' where \emph{i} is the number of different alleles and \eqn{\left( \begin{array}{c} i \\ 2 \end{array} \right)} is
#' \emph{i} choose 2.
#' The object is called Fig3_1 as it is Figure 3.1 on page 55 of Mielke, Konigsberg,
#' and Relethford (2011) and on page 52 of the 2006 edition.
#' @param start Starting number of alleles. Default=5
#' @param stop Ending number of alleles. Default=50
#' @return Returns plot of heterozygosity against number of alleles.
#' @references
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2011)
#' \emph{Human biological variation}, 2nd ed.  New York: Oxford University Press.
#'
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2006)
#' \emph{Human biological variation}, 1st ed.  New York: Oxford University Press.
#' @export
Fig3_1=function (start=5,stop=50)
{
  H=0
  for(i in start:stop) H[i-start+1] = 2*choose(i,2)*(1/i)^2
  plot(start:stop,H,type='b',xlab='Number of Alleles',
       ylab='Frequency of Heterozygotes')
}
