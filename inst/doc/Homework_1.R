## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=F-------------------------------------------------------------------
#  # Always start your homework by copying and pasting the line below
#  library(Anth241Hmwk)

## ----eval=FALSE---------------------------------------------------------------
#  # run entering the suggested phenotype counts
#  # and copy output (Obs & Exp and $N, $p, $q, and $r) and paste
#  # to your document
#  P_56()

## ----eval=FALSE---------------------------------------------------------------
#  # run entering whatever phenotype counts you want
#  # and copy output (Obs & Exp) and paste
#  # to your document
#  sto=P_56()

## ----eval=FALSE---------------------------------------------------------------
#  # This time you did not see the $N, $p, $q, and $r. Why is
#  # that? It is because they got stored in "sto." To see them
#  # copy and paste the line below to your console:
#  sto

## ----eval=FALSE---------------------------------------------------------------
#  # Paste this output to your document.  It should be
#  # the same as from the "expected" value for the AB phenotype
#  sto$N*2*sto$p*sto$q
#  

## ----eval=FALSE---------------------------------------------------------------
#  # Run this and copy/paste console output and graph to your document
#  Tab3_2()

## ----eval=FALSE---------------------------------------------------------------
#  # Copy/paste the two lines below to setup the allele frequencies
#  p=1/3
#  q=1-p # remember, in bi-allelic system q=1-p

## ----eval=FALSE---------------------------------------------------------------
#  # Run the line below and copy/paste output to document
#  Tab3_2(100*p^2,100*2*p*q,100*q^2) # This is in H-W equil, so p=1

## ----eval=FALSE---------------------------------------------------------------
#  # Run the line below and copy/paste output to document
#  Tab3_2(100*p^2,100*2*p*q,50*q^2)

## ----eval=FALSE---------------------------------------------------------------
#  # Run the line below and copy/paste output to document
#  Tab3_2(100*p^2,100*2*p*q,10*q^2)

## ----eval=FALSE---------------------------------------------------------------
#  # Run and copy/paste the graph to your document
#  Fig3_9(p.start=runif(100))

## ----eval=FALSE---------------------------------------------------------------
#  # Run me and copy/paste the plot to your document
#  Fig3.9(c(1,.6,1),runif(100))

## ----eval=FALSE---------------------------------------------------------------
#  # Balanced polymorphism
#  Fig3_13()

## ----eval=FALSE---------------------------------------------------------------
#  # Heterozygote disadvantage
#  Fig3_13(c(1,.6,1),c(0,1),c(0,1),tan=.05)

## ----eval=FALSE---------------------------------------------------------------
#  Fig3_17(10^0,jit=0.1) # i.e., deme sizes are all = 1

## ----eval=FALSE---------------------------------------------------------------
#  Fig3_17(10^1) # i.e., deme sizes are all = 10

## ----eval=FALSE---------------------------------------------------------------
#  Fig3_17(10^2) # i.e., deme sizes are all = 100

## ----eval=FALSE---------------------------------------------------------------
#  Fig3_17(10^3) # i.e., deme sizes are all = 1,000

## ----eval=FALSE---------------------------------------------------------------
#  Fig3_17(10^4) # i.e., deme sizes are all = 10,000

## ----eval=FALSE---------------------------------------------------------------
#  Fig3_20() # and answer 5 for number of A alleles

## ----eval=FALSE---------------------------------------------------------------
#  # This one will "drift slower", but you would still be able to
#  # order the graphs
#  Fig3_20(5) # and answer 10 for number of A alleles

