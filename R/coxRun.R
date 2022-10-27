coxRun=function(dat){
  library(survival)
  colnames(dat)=c('time','status','AS')
  dat=dat[which(!is.na(dat[,1])&!is.na(dat[,3])&!is.na(dat[,2])),]
  #print(nrow(dat))
  if(nrow(dat)<10){
    print(paste0('Sample Num is small:',nrow(dat)))
    return(c(NA,NA,NA,NA))
  }
  #if(quantile(dat[,3])['25%']==quantile(dat[,3])['50%']) return(c(NA,NA,NA,NA))
  fmla <- as.formula("Surv(time, status) ~AS")
  if(table(dat[,2])[1]>1&table(dat[,2])[2]>1){
    cox <- survival::coxph(fmla, data = dat)
    re=c(summary(cox)[[7]][5],summary(cox)[[7]][2],summary(cox)[[8]][3],summary(cox)[[8]][4])
    return(re)
  }else{
    return(c(NA,NA,NA,NA))
  }
}
