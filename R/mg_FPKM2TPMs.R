mg_FPKM2TPMs=function(exp){
  exp1=exp
  exp1[is.na(exp1)]=0
  ct=apply(exp1, 2, sum)
  exp1=t(t(exp1)/ct)
  exp1=exp1*1e6
  return(exp1)
}
