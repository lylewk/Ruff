#' @export
Fig11_14=function(){
  opar = par(no.readonly = T)
  on.exit(par(opar))
  if(.Platform$OS.type=='windows') quartz=function() windows()
  quartz()

  par(mfrow=c(2,2))
  Fig11_14_util(1)
  Fig11_14_util(2)
  Fig11_14_util(3)
  Fig11_14_util(4)
}
