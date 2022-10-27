mg_cal_cox_auc_cindex=function(riskscore,event,os){
  dat=data.frame(riskscore=as.numeric(riskscore),status=as.numeric(event),time=as.numeric(os))
  dat=dat[which(!is.na(os)&!is.na(event)),]
  if(nrow(dat)<3){
    aucs=c(NA,NA,NA,NA,NA,NA,NA,NA,NA)
    all.mg=c(c(NA,NA,NA,NA,NA,NA,NA,NA),aucs)
    names(all.mg)=c('HR.pvalue','HR','HR_lower','HR_upper'
                    ,'Cindex.pvalue','Cindex','Cindex_lower','Cindex_upper'
                    ,'1-year.AUC','1-year.AUC_lower','1-year.AUC_upper'
                    ,'3-year.AUC','3-year.AUC_lower','3-year.AUC_upper'
                    ,'5-year.AUC','5-year.AUC_lower','5-year.AUC_upper'
    )
    return(all.mg)
  }
  fmla <- as.formula(paste0("Surv(time, status) ~ riskscore"))
  cox <- coxph(fmla, data = dat)
  hrs=c(summary(cox)[[7]][5],summary(cox)[[7]][2],summary(cox)[[8]][3],summary(cox)[[8]][4])
  c1=survcomp::concordance.index(x=dat[,1], surv.time=dat[,3], surv.event=dat[,2],
                                 method="noether")
  cdx=c(c1$p.value,c1$c.index,c1$lower,c1$upper)
  auc=mg_time_roc(time = dat[,3],status = dat[,2],score = dat[,1])
  aucs=c()
  if(nrow(auc)>2){
    aucs=c(auc[1,],auc[2,],auc[3,])
  }else if (nrow(auc)>1){
    aucs=c(auc[1,],auc[2,],NA,NA,NA)
  }else if (nrow(auc)>0){
    aucs=c(auc[1,],NA,NA,NA,NA,NA,NA)
  }else{
    aucs=c(NA,NA,NA,NA,NA,NA,NA,NA,NA)
  }
  all.mg=c(hrs,cdx,aucs)
  names(all.mg)=c('HR.pvalue','HR','HR_lower','HR_upper'
                  ,'Cindex.pvalue','Cindex','Cindex_lower','Cindex_upper'
                  ,'1-year.AUC','1-year.AUC_lower','1-year.AUC_upper'
                  ,'3-year.AUC','3-year.AUC_lower','3-year.AUC_upper'
                  ,'5-year.AUC','5-year.AUC_lower','5-year.AUC_upper'
  )
  return(all.mg)
}
