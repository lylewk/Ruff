#' @export
plot_tree = function (phy,mut.rate=0,x.offset=0,muts=T)
{
  N <- length(phy$tip.label)
  x.pos = c(1:N,rep(NA,N-2))+x.offset
  y.pos = rep(0,2*N-1)

  for(i in seq(2*N-1,N+1,-1)){
    parent = which(phy$edge[,1]==i)
    point=phy$edge[parent,2]
    x.pos[i]=mean(x.pos[point])

    parent = parent[1]
    child=phy$edge[parent,2]
    height=phy$edge.length[parent]
    y.pos[i]=y.pos[i]+height

    while(child>N){
      parent = which(phy$edge[,1]==child)[1]
      child=phy$edge[parent,2]
      height=phy$edge.length[parent]
      y.pos[i]=y.pos[i]+height
    }
  }

  nodes=cbind(x.pos,y.pos)
  max.depth=max(nodes[,2])*1.2
  mut.rate=15/max.depth

  top=phy$edge[1,1]
  lines(rep(nodes[top,1],2),c(10^10,nodes[top,2]))
  sto.muts=0
  for(i in 1:(2*N-2)){
    top=phy$edge[i,1]
    bot=phy$edge[i,2]
    lines(c(nodes[top,1],nodes[bot,1],nodes[bot,1]),
          c(nodes[top,2],nodes[top,2],nodes[bot,2]))
    if(muts==T){
      n.mut=rpois(1,phy$edge.length[i]*mut.rate)
      sto.muts[i]=n.mut
      if(n.mut>0){
        y.top=nodes[top,2]
        y.bot=nodes[bot,2]
        nudge=(y.top-y.bot)/7
        y=runif(n.mut,y.bot+nudge,y.top-nudge)
        points(rep(nodes[bot,1],n.mut),y,pch=20)
      }

    }
  }
  if(muts==F) return(NULL)

  ancestors=mrca(phy)[1:36,1:36]
  freqs=rep(0,60)
  for(i in 1:35){
    for(j in (i+1):36){
      common=ancestors[i,j]

      diffs=0

      diffs = diffs + sto.muts[which(phy$edge[,2]==i)]
      parent.L = phy$edge[which(phy$edge[,2]==i),1]
      while(parent.L!=common) {
        diffs = diffs + sto.muts[which(phy$edge[,2]==parent.L)]
        parent.L = phy$edge[which(phy$edge[,2]==parent.L),1]
      }

      diffs = diffs + sto.muts[which(phy$edge[,2]==j)]

      parent.R = phy$edge[which(phy$edge[,2]==j),1]
      while(parent.R!=common) {
        diffs = diffs + sto.muts[which(phy$edge[,2]==parent.R)]
        parent.R = phy$edge[which(phy$edge[,2]==parent.R),1]
      }
      freqs[diffs+1]=freqs[diffs+1]+1
    }
  }
  if(.Platform$OS.type=='windows') quartz=function() windows()
  quartz()
  dev.next()
  tot=sum(freqs)
  running=cumsum(freqs)
  top=min(which(running==tot))
  y=freqs[1:top]/sum(freqs)*100
  my_hist(y)
}
