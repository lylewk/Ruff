#' @export
my_hist = function (y)
{
  n.rects=length(y)
  plot(c(-.5,n.rects),c(0,max(y)),type='n',axes=F,xlab='No. of Differences',ylab='Percent')
  for(i in 1:n.rects){
    rect(i-1.5,0,i-.5,y[i],col='gray')
  }
  axis(1,at=0:100,pos=0)
  axis(2)
}
