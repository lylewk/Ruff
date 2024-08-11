#' @export
Fig3_17=function (N=10,gens=100,jit=0)
{
  demes=100
  sim.line=function(){
    p=0
    p[1]=0.5
    for(i in 2:gens){
      p[i]=rbinom(1,2*N,p[i-1])/(2*N)
    }
    return(p)
  }

  p=sim.line()
  plot(1:gens,jitter(p,jit),type='l',xlab='Generation',ylab='p',ylim=c(0,1),main=paste('N = ',N))
  for(i in 2:demes){
    p=sim.line()
    lines(1:gens,jitter(p,jit))
  }
}
