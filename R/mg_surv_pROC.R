mg_surv_pROC=function(time,status,score,mks=c(1,3,5)){
  mks=mg_predict_time_ymd(time,mks)
  library(pROC)
  library(survival)
  dat=data.frame(time,status,score)
  dat=dat[!is.na(time)&!is.na(status)&!is.na(score),]
  colnames(dat)=c('time','status','score')
  plots=list()
  AUCs=c()
  cols=mg_colors
  print(mks)
  for (mk in mks) {
    dat1=dat
    dat1$year=rep(NA,nrow(dat))
    dat1$year[dat1$time >=mk & dat1$status == 0] =0
    dat1$year[dat1$time >=mk & dat1$status == 1] =0
    dat1$year[dat1$time < mk & dat1$status == 0] =NA
    dat1$year[dat1$time < mk & dat1$status == 1] =1
    dat1=dat1[which(!is.na(dat1$year)),]
    #print(head(dat1))
    #print(length(plots)>0)
    pt=mg_surv_pROC_smooth_trycatch(dat1$year,dat1$score,cols[length(plots)+1],add = length(plots)>0)
    #print(pt)
    AUCs=c(AUCs,round(pt$auc,2))
    #print(AUCs)
    plots=c(plots,list(pt))

  }
  if(max(mks)<20){
    lb=paste0(mks,'-Years')
  }else if(max(mks)<365){
    lb=paste0(round(mks/12,0),'-Years')
  }else{
    lb=paste0(round(mks/365,0),'-Years')
  }
  legend('bottomright',paste0(lb,',AUC=',AUCs),col = cols[1:length(AUCs)],
         lty = c(1,1),border = '',title = 'AUC',bty="n")
  return(plots)
}
