#' @export
Fig11_07 = function ()
{
  dacount=structure(list(age = c(0, 0.50137, 1.00274, 1.50411, 2.00274,
                                 2.50411, 3.00274, 3.50411, 4.00274, 4.589041, 5.005479, 5.591781,
                                 6.005479, 6.558904, 7.005479, 7.254795, 7.506849, 8.005479, 8.506849,
                                 9.008219, 9.627397, 10.00822, 11.50959, 12.00822, 12.67671, 13.01096,
                                 13.51233, 14.01096, 14.53973, 15.01644, 15.53425, 16.2, 16.52877,
                                 17.01918, 17.12055, 17.44658, 17.6), height = c(51.4, 65, 73.1,
                                 81.2, 90, 92.8, 98.8, 100.4, 105.2, 109.5, 111.7, 115.5, 117.8,
                                122.9, 124.3, 127, 128.9, 130.8, 134.4, 137, 140.1, 141.9, 148.6,
                                149.9, 154.1, 155.3, 158.6, 162.9, 169.2, 175, 177.5, 181.4,
                                183.3, 184.6, 185.4, 186.5, 186.8)), .Names = c("age", "height"),
                                class = "data.frame", row.names = c(NA, -37L))
  pbheight.fm<<-height ~ h1 - 2 * (h1 - hc)/(exp(s0 * (age - c)) + exp(s1 * (age-c)))
  pbheight.st=structure(list(h1 = 180, s0 = 0.1, s1 = 1, c = 15, hc = 120),
                        .Names = c("h1","s0", "s1", "c", "hc"), row.names = c(NA, -1L), class = "data.frame")
  pbheight.dist<<-nls(pbheight.fm,data=dacount,start=pbheight.st)
  plot(dacount,xlab='Age',ylab='Stature (cm)',main="Count de Montbeillard's son")
  lines(seq(0,20,.1),predict(pbheight.dist,newdata=list(age=seq(0,20,.1))))
}
