#' @export
Fig11_08b = function ()
{
  env=new.env()
  assign('age',seq(0,20,.1),envir=env)
  parms=coef(pbheight.dist)
  assign('h1',parms[1],envir=env)
  assign('s0',parms[2],envir=env)
  assign('s1',parms[3],envir=env)
  assign('c',parms[4],envir=env)
  assign('hc',parms[5],envir=env)
  pbheight.fm=expression(h1 - 2 * (h1 - hc)/(exp(s0 * (age - c)) + exp(s1 * (age -
                                                                               c))))

  dd=D(D(pbheight.fm,'age'),'age')

  y=eval(dd,env)
  plot(seq(0,20,.1),y,type='l',xlab='Age',ylab='Change in velocity',
       main='Acceleration Curve')
  abline(h=0)
}
