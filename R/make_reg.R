make_reg= function(){
  regs=matrix(NA,nr=4,nc=3)
  regs[,1]=V.2015[1,1]/V.2015[1,2:5]
  regs[,2]=mu[1]-mu[2:5]*regs[,1]
  r=vector()
  for(i in 1:4){
    r[i]=V.2015[1,i+1]/sqrt(V.2015[1,1]*V.2015[i+1,i+1])
  }
  see=vector()
  see=sqrt(V.2015[1,1]*(r^(-2)-1))
  regs[,3]=see
  return(regs)
}
