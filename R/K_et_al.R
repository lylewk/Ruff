K_et_al_Fem=function(){

est.stat=function(Fem){
  stat=3.4751*Fem+105.58
  lo=-.6745*48.65
  hi=-lo
  return(round(c(lo+stat,stat,stat+hi)))
}

dat=Europe2[complete.cases(Europe2[,8]),8]
N=NROW(dat)
out=matrix(NA,nc=3,nr=N)
for(i in 1:N){
  out[i,]=est.stat(dat[i])
}
out=data.frame(Europe2$Stat,out)
colnames(out)=c('actual','lo.25','est','hi.25')
windows()
plot(out$actual,out$est)
abline(0,1)

lo.quarter=round(0.25*N)
hi.quarter=round(0.75*N)
mid.half=c(lo.quarter+1,hi.quarter-1)
lo.quarter=c(1,lo.quarter)
hi.quarter=c(hi.quarter,N)

grouped=vector()
grouped[lo.quarter[1]:lo.quarter[2]]=1
grouped[mid.half[1]:mid.half[2]]=2
grouped[hi.quarter[1]:hi.quarter[2]]=3

below=rep(FALSE,N)
with.in=rep(FALSE,N)
above=rep(FALSE,N)
for(i in 1:N){
  if(out$actual[i]<out$lo.25[i]) below[i]=TRUE
  if(out$actual[i]>=out$lo.25[i] & out$actual[i]<=out$hi.25[i]) with.in[i]=TRUE
  if(out$actual[i]>out$hi.25[i]) above[i]=TRUE
}

in.groups=as.numeric(table(grouped))
print(sum(below[grouped==1])/in.groups[1])
print(sum(below[grouped==2])/in.groups[2])
print(sum(below[grouped==3])/in.groups[3])
cat('\n')
print(sum(with.in[grouped==1])/in.groups[1])
print(sum(with.in[grouped==2])/in.groups[2])
print(sum(with.in[grouped==3])/in.groups[3])
cat('\n')
print(sum(above[grouped==1])/in.groups[1])
print(sum(above[grouped==2])/in.groups[2])
print(sum(above[grouped==3])/in.groups[3])
cat('\n')
sum(with.in)/N
}
