#' @export
Fig11_14_util = function(ecc=1)
{
  library(scatterplot3d)
  edge=2
  cube <- rbind(c(0,0,1), 0, c(1,0,0), c(1,1,0),1, c(0,1,1), # < 6 outer
                c(1,0,1), c(0,1,0)) # <- "inner": fore- & back-ground
  vert=ecc*edge
  horz=sqrt(8/vert)
  cub=cube*(rep(1,8)%o%c(horz,horz,vert))
  cube=scatterplot3d(cub[c(1:6,1,7,3,7,5) ,], type = 'l',box=F,axis=F,xlab='',
                     ylab='',zlab='',grid=F,lty.hide=2,xlim=c(0,8),ylim=c(0,8),zlim=c(0,8))
  cube$points3d(cub[c(2,8,4,8,6),     ], type = 'l',lty=3)
  mtext(side=1,adj=0,
        bquote('Surface area'==.(4*vert*horz+2*horz^2)~cm^2))
  mtext(side=1,adj=0,line=1,
        bquote('Volume'==.(horz^2*vert)~cm^3))
  mtext(side=1,adj=0,line=2,
        bquote('SA/Vol'==.((4*vert*horz+2*horz^2)/(horz^2*vert))))
}
