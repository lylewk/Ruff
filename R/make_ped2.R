#' @export
make_ped2=function ()
{
  library(optiSel)
  par(cex=0.75)
  Indiv=c('Aegon V','Unknown','Aerys II','Rhaella','Rhaegar','Daenerys','Lyanna','Jon','Jon II')
  Sire=c(NA,NA,'Aegon V','Aegon V','Aerys II','Aerys II',NA,'Rhaegar','Jon')
  Dam=c(NA,NA,'Unknown','Unknown','Rhaella','Rhaella',NA,'Lyanna','Daenerys')
  sex=c('M','F','M','F','M','F','F','M','M')
  Pedigree=data.frame(Indiv=Indiv,Sire=Sire,Dam=Dam,Sex=sex)

  pedplot(Pedigree)
  pedInbreeding(Pedigree)
}
