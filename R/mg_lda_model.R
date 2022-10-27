mg_lda_model=function(dat,group){
  library(MASS)
  irisdata = dat
  irisgrp = group
  lda.sol = lda(irisdata, irisgrp)
  pred=predict(lda.sol,irisdata, probability=TRUE, decision.values=TRUE)
  return(list(Model=lda.sol,predict=pred$class))
}
