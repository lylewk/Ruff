#' @title Albanese et al. (2016) stature regressions
#' @export
S_run=function(i.which='Fem'){
  data(Europe2)
  bones=c('Fem','Tib','Hum','Rad')
  Row = which(i.which==bones)
  parms=S_reg[Row,]
  dat = Europe2[,c(7,7+Row)]
  run_it(parms,dat,i.which,'Sjovold (1990),')
}
