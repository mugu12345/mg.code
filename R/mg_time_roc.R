mg_time_roc=function(time,status,score,mks=c(1,3,5)){
  library(survival)
  omks=mks
  #quantile(time,seq(0,1,0.01))
  #sum(time>max(mks))
  mks=mg_predict_time_ymd(time,mks)
  ROC.DSST=timeROC::timeROC(T=time,
                            delta=status
                            ,marker=score,
                            cause=1,weighting="marginal",
                            times=mks,
                            iid=TRUE)
  dat=cbind(AUC=ROC.DSST$AUC,Lower=confint(ROC.DSST,level = 0.95,na.rm=T)$CI_AUC[,1]/100
            ,Upper=confint(ROC.DSST,level = 0.95,na.rm=T)$CI_AUC[,2]/100)
  if(nrow(dat)<length(omks)){
    dat=rbind(dat,c(AUC=rep(NA,length(omks)-nrow(dat)),
                    Lower=rep(NA,length(omks)-nrow(dat)),
                    Upper=rep(NA,length(omks)-nrow(dat))))
  }
  row.names(dat)=paste0(omks,'-years')[1:nrow(dat)]
  return(dat)
}
