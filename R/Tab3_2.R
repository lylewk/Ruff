#' @export
Tab3_2=function (AA=136,AB=66,BB=17)
  {
    AA=round(AA,0)
    AB=round(AB,0)
    BB=round(BB,0)
    options(warn=-1)
    N=AA+AB+BB
    Two.N=2*N
    A=2*AA+AB
    B=2*BB+AB
    f.A=(2*AA+AB)/Two.N
    f.B=1-f.A
    cat('\nObserved values:\n')
    cat(c(AA,AB,BB))
    cat('\n')
    cat('Expected values:\n')
    cat(c(f.A^2*N,2*f.A*f.B*N,f.B^2*N))
    cat('\n')
    p = function(AA,AB,BB) return(exp(lgamma(N+1)+lgamma(A+1)+lgamma(B+1)+log(2)*AB
                                      -lgamma(AA+1)-lgamma(AB+1)-lgamma(BB+1)-lgamma(Two.N+1)))
    start.AA=(A-B)/2
    stop.AA=A/2
    sto.p=0
    obs.p=p(AA,AB,BB)
    for(i in start.AA:stop.AA){
      t.AA=i
      t.AB=A-2*t.AA
      t.BB=N-t.AA-t.AB
      sto.p[i-start.AA+1]=p(t.AA,t.AB,t.BB)
    }
    stop.AA=(start.AA:stop.AA)[max(which(sto.p>1.e-8))]
    top=max(sto.p)*1.1
    plot(c(start.AA-.5,stop.AA+0.5),c(0,top),type='n',xlab='Number of AA genotypes',
         ylab='Probability')
    tot.p=0
    for(i in start.AA:stop.AA){
      curr=i-start.AA+1
      if(sto.p[curr]<=obs.p) {
        tot.p=tot.p+sto.p[curr]
        rect(i-.5,0,i+.5,sto.p[curr],density=40)
      }
      else rect(i-.5,0,i+.5,sto.p[curr])
    }
    lines(c(AA,AA),c(0,1))
    cat('p-value:\n')
    cat(round(tot.p,6))
    cat('\n')
  }
