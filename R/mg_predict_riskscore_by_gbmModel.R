mg_predict_riskscore_by_gbmModel=function(gbmModel,data,method = "OOB"){
  m.gbm=gbmModel$model
  genes=gbmModel$Genes
  dat=data[,match(genes,colnames(data))]
  colnames(dat)=paste0('g',1:ncol(dat))
  best.iter <- gbm.perf(m.gbm, method = method )
  riskscore=predict.gbm(m.gbm,dat,n.trees = best.iter)
  return(riskscore)
}

