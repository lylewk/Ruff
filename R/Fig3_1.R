#' @export
Fig3_1=function (start=5,stop=50)
{
  H=0
  for(i in start:stop) H[i-start+1] = 2*choose(i,2)*(1/i)^2
  plot(start:stop,H,type='b',xlab='Number of Alleles',
       ylab='Frequency of Heterozygotes')
}
