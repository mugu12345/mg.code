mg_merge_cpgBeta_by_pvalue=function(beta,pdata,cutoff=0.01){
  for(i in 1:ncol(beta)){
    beta[which(pdata[,i]>cutoff),i]=NA
  }
  return(beta)
}
