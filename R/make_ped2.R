#' @title Plot pedigree from Game of Thrones and calculate inbreeding coefficents
#' @description Family tree for "Jon II," the presumed son of Jon Snow (Stark) and
#' Daenerys Targaryn.
#' @details OK, I got this a few years back from what I thought was the definitive
#' family tree.  I'm not so sure now about some of the links in this tree.  There
#' are no parameters as this is just a "demo" that is "hard-wired" to draw this
#' particular tree.
#' @return Returns pedigree and inbreeding coefficients.  We will see
#' in the future that the inbreeding coefficient is the probability of
#' identity by descent (i.b.d.), the probability that the alleles are
#' the same at a locus because they were inherited from the same ancestor(s).
#' @export
make_ped2=function ()
{
  par(cex=0.75)
  Indiv=c('Aegon V','Unknown','Aerys II','Rhaella','Rhaegar','Daenerys','Lyanna','Jon','Jon II')
  Sire=c(NA,NA,'Aegon V','Aegon V','Aerys II','Aerys II',NA,'Rhaegar','Jon')
  Dam=c(NA,NA,'Unknown','Unknown','Rhaella','Rhaella',NA,'Lyanna','Daenerys')
  sex=c('M','F','M','F','M','F','F','M','M')
  Pedigree=data.frame(Indiv=Indiv,Sire=Sire,Dam=Dam,Sex=sex)

  optiSel::pedplot(Pedigree)
  sto=optiSel::pedInbreeding(Pedigree)
  row.names(sto)=NULL
  return(sto)
}
