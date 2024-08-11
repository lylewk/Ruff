#' @export
Fig3_9=function (w=c(0.7635,1,0.0001),p.start=c(0.0001,0.9999))
{
  p=0.0001
  t=0:42
  w.AA=w[1]
  w.AS=w[2]
  w.SS=w[3]

  p[1]=p.start[1]
  for(i in 2:43){
    ave.w=p[i-1]^2*w[1]+2*p[i-1]*(1-p[i-1])*w[2]+(1-p[i-1])^2*w[3]
    p[i]=(p[i-1]^2*w[1]+p[i-1]*(1-p[i-1])*w[2])/ave.w
  }
  plot(t,p,type='l',xlim=c(0,40),ylim=c(0,1),xlab='Generation',
       ylab='Allele Frequency (A)')

  n.starts=length(p.start)
  for(j in 2:n.starts){
    p[1]=p.start[j]
    for(i in 2:43){
      ave.w=p[i-1]^2*w[1]+2*p[i-1]*(1-p[i-1])*w[2]+(1-p[i-1])^2*w[3]
      p[i]=(p[i-1]^2*w[1]+p[i-1]*(1-p[i-1])*w[2])/ave.w}
    lines(t,p)
  }
}
