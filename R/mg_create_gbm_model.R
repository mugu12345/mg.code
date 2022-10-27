mg_create_gbm_model=function(dat,time,event,tree=1000,interaction.depth=2,shrinkage=0.01){
  cls=colnames(dat)
  dat1=cbind(dat,time,event)
  colnames(dat1)=c(paste0('g',1:ncol(dat)),'time','status')
  dat1=as.data.frame(dat1)
  library(gbm)
  m.gbm <- gbm(Surv(time, status)~.,dat1,interaction.depth=interaction.depth,
               shrinkage=shrinkage,n.trees=tree,distribution="coxph")
  riskscore=mg_predict_riskscore_by_gbmModel(list(model=m.gbm,Genes=cls))
  return(list(model=m.gbm,Genes=cls,Score=riskscore))
}
