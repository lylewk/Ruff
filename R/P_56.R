#' @export
P_56=function(){
  cat('Enter the number of individuals with type-A blood (179 in Taylor & Prior)', fill=F ); A = scan(n=1,quiet=T)
  cat('Enter the number of individuals with type-B blood (35 in T & P)', fill=F ); B = scan(n=1,quiet=T)
  cat('Enter the number of individuals with type-AB blood (6 in T & P)', fill=F ); AB = scan(n=1,quiet=T)
  cat('Enter the number of individuals with type-O blood (202 in T & P)', fill=F ); O = scan(n=1,quiet=T)

  p=0;q=0;r=0
  N=A+B+AB+O
  r[1]=sqrt(O/N)
  p[1]=(1-r[1])/2
  q[1]=p[1]
  for(i in 2:10){
    AA=p[i-1]^2/(p[i-1]^2+2*p[i-1]*r[i-1])*A
    AO=A-AA
    BB=q[i-1]^2/(q[i-1]^2+2*q[i-1]*r[i-1])*B
    BO=B-BB
    p[i]=(2*AA+AO+AB)/(2*N)
    q[i]=(2*BB+BO+AB)/(2*N)
    r[i]=1-p[i]-q[i]
  }
  ABO = matrix(c(A,(p[10]^2+2*p[10]*r[10])*N,B,(q[10]^2+2*q[10]*r[10])*N,
                 AB,(2*p[10]*q[10])*N,O,r[10]^2*N),nc=2,byrow=T)
  colnames(ABO)=c('Obs','Exp')
  rownames(ABO)=c('A','B','AB','O')
  cat('\n')
  print(ABO)
  cat('\n')
  return(list(N=N,p=p[10],q=q[10],r=r[10]))
}
