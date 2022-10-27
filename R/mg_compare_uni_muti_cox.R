mg_compare_uni_muti_cox=function(dat,event,os){
  dat=crbind2DataFrame(dat)
  sig.clini=colnames(dat)
  dat$time=os
  dat$status=event
  dat=dat[which(!is.na(os)&!is.na(event)),]
  all.cox=rbind()
  rnames=c()
  for(s in sig.clini){
    fmla <- as.formula(paste0("Surv(time, status) ~",s))
    cox <- coxph(fmla, data = dat)
    #summary(cox)[[7]]
    #print(summary(cox))
    re=cbind(summary(cox)[[7]][,5],summary(cox)[[7]][,2],summary(cox)[[8]][,3],summary(cox)[[8]][,4])
    if(nrow(re)==1){
      rnames=c(rnames,s)
    }else{
      rnames=c(rnames,row.names(summary(cox)[[7]]))
    }
    all.cox=rbind(all.cox,re)
  }
  row.names(all.cox)=rnames
  colnames(all.cox)=c('p.value','HR','Low 95%CI','High 95%CI')

  fmla <- as.formula(paste0("Surv(time, status) ~",paste0(sig.clini,collapse = '+')))
  cox <- coxph(fmla, data = dat)
  muti.re=cbind(summary(cox)[[7]][,5],summary(cox)[[7]][,2],summary(cox)[[8]][,3],summary(cox)[[8]][,4])
  row.names(muti.re)=row.names(summary(cox)[[7]])
  colnames(muti.re)=c('p.value','HR','Low 95%CI','High 95%CI')
  return(list(muti=crbind2DataFrame(muti.re),uni=crbind2DataFrame(all.cox)))
}
