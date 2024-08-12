#' @title Preece-Baines acceleration curve from longitudinal growth data
#' @description Takess a Preece-Baines growth curve to fit data given in Scammon (1927)
#' and finds the second derivative to get the accelerative curve.
#' @details
#' The object is called Fig11_08b as it gives an acceleration curve which would
#' have come from Figure 11.8 on page 271 of Mielke, Konigsberg, and Relethford
#' (2011) and on page 261 of the 2006 edition from Figure 10.8.  Unfortunately,
#' neither edition had the acceleration curve. It plots the smooth acceleration
#' curve by finding the second derivative of the Preece-Baines
#' (1978) curve using data from Scammon (1927).  \strong{Note}: You must run Fig11_7()
#' as it write the Preece-Baines parameters into the work space.
#' @return Plot of acceleration curve.
#' @references
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2011)
#' \emph{Human biological variation}, 2nd ed.  New York: Oxford University Press.
#'
#' Mielke, James H., Konigsberg, Lyle W., and Relethford, John H. (2006)
#' \emph{Human biological variation}, 1st ed.  New York: Oxford University Press.
#'
#' Preece, M. A., & Baines, M. J. (1978). A new family of mathematical models
#' describing the human growth curve. \emph{Annals of Human Biology}, 5, 1-24.
#'
#' Scammon, R. E. (1927). The first seriatim study of human growth.
#' \emph{American Journal of Physical Anthropology}, 10(3), 329-336.
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
