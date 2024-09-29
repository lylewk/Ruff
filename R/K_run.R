#' @title Konigsberg & Jantz (2018) stature regressions
#' @export
K_run=function(i.which='Fem'){
  data(Europe2)
  bones=c('Fem','Tib','Hum','Rad')
  Row = which(i.which==bones)
  parms=K_reg[Row,]
  dat = Europe2[,c(7,7+Row)]
  run_it(parms,dat,i.which,'Konigsberg & Jantz (2018),')
}
