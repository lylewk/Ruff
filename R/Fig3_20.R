#' @title Deterministic simulation of genetic drift
#' @description Shows histograms of number of A alleles at various generations across
#' sub-populations where the number of A alleles are subject to genetic drift.
#' @details
#' The object is called Fig3_20 as it is Figure 3.20 on page 79 of Mielke, Konigsberg,
#' and Relethford (2011) and on page 75 of the 2006 edition where it is Figure 3.15.
#' The plot draws the deterministic histogram of sub-population A allele counts at
#' various generations where these counts are subject to genetic drift because of
#' finite sub-population size.  The plot always shows the allele count at generation
#' 0 and then at the generations specified in "gen."  From the input "Nids" the script will
#' ask how many of the 2xNids alleles you want to be A alleles at the "dawn of time."
#' @param Nids An integer giving the population size for each sub-population. Default = 5
#' @param gens A vector of four generations at which to view the histograms.
#' Default=c(5,10,20,30)
#' @return Plot of histograms of A allele counts at generation 0 and at five other
#' generations specified in "gens".
#' @references
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2011)
#' \emph{Human biological variation}, 2nd ed.  New York: Oxford University Press.
#'
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2006)
#' \emph{Human biological variation}, 1st ed.  New York: Oxford University Press.
#' @export
Fig3_20=function (Nids=5,gens=c(5,10,20,30))
{
  opar = par(no.readonly = T)
  on.exit(par(opar))
  if(.Platform$OS.type=='windows') quartz=function() windows()
  quartz()
  par(mfrow=c(3,2))
  my.hist=function(label){
    plot(0:N.alleles,vec,xlim=c(-.5,N.alleles+.5),ylim=c(0,max(vec)),
         type='n',axes=F,xlab='Number of A alleles',ylab='Frequency')
    if(label==1) {title(main=paste(label,' Generation'))}
    else {title(main=paste(label,' Generations'))}
    axis(1)
    axis(2)
    for(i in 1:states){
      rect(i-1.5,0,i-.5,vec[i])
    }
  }
  N.alleles=2*Nids
  cat('\nThere are ',N.alleles,' alleles per deme')
  cat('\nHow many alleles should be A alleles at the dawn of time?')
  cat('\nBe sure you are at the "1:" prompt\n')
  N.start=scan(n=1)
  cat('\nClick on graphics window to see results (note: this may
      be at the bottom as an icon)\n')
  states=N.alleles+1
  Tr=diag(states)
  for(i in 1:states){
    p=(i-1)/N.alleles
    Tr[i,]=dbinom(0:N.alleles,N.alleles,p)
  }
  vec=c(rep(0,N.start),1,rep(0,states-N.start-1))
  my.hist(0)
  vec=as.vector(vec%*%Tr)
  my.hist(1)
  i.count=1
  for(i in 1:gens[4]){
    vec=as.vector(vec%*%Tr)
    if(i==gens[i.count]){
      my.hist(gens[i.count])
      i.count=i.count+1
    }
  }
}
