#' @export
Fig3_13=function (w=c(0.7635,1,0.0001),x=c(.65,.95),y=c(0.77,0.82),
                  tan.int=.01)
{
  w.AA=w[1]
  w.AB=w[2]
  w.BB=w[3]

  p=seq(0.01,0.99,0.01)
  ave.w = function(p) return(p^2*w[1]+2*p*(1-p)*w[2]+(1-p)^2*w[3])
  slope.w=function(p){
    return(2*(p*(w.AA-w.AB)+(1-p)*(w.AB-w.BB)))
  }
  plot(p,ave.w(p),type='l',xlim=x,ylim=y)
  tangent=function(p){
    b=slope.w(p)
    a=ave.w(p)-p*b
    abline(a,b)
  }
  tans=seq(x[1],x[2],tan.int)
  n=length(tans)
  for(i in 1:n) tangent(tans[i])
}
