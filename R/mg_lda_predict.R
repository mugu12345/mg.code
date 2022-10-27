mg_lda_predict=function(lda_Model,dat,group){
  #lda_Model=exp.lda$Model
  #dat=scale(t(exp.limma$Exp[match(gv33.info$gene_id,row.names(exp.limma$Exp)),]))
  #group=exp.limma$Group

  pred=predict(lda_Model,dat, probability=TRUE, decision.values=TRUE)

  P = lda_Model$scaling
  # 将均值向量降维
  means = lda_Model$means %*% P
  # 加权平均的出总的降维均值向量，权重就是lda.sol$prior
  total_means = as.vector(lda_Model$prior %*% means)
  #lda_Model$N
  n_samples = nrow(dat)
  # 把样本降维并平移
  x<-as.matrix(dat) %*% P - (rep(1, n_samples) %o% total_means)
  score=as.numeric(apply(x,1, sum))
  all_auc=c()
  for(g in unique(group)){
    roc.pred <- ROCR::prediction (score,labels = ifelse(group==g,g,'Other'))
    perf <- ROCR::performance (roc.pred, "tpr", "fpr")
    auc=ROCR::performance(roc.pred,'auc')@y.values[[1]]
    predictions= roc.pred@predictions[[1]]
    label=roc.pred@labels[[1]]

    dt=list(fpr = perf@x.values [[1]], tpr = perf@y.values [[1]]
            ,predictions= predictions
            ,label=label
            ,threshold = perf@alpha.values [[1]],
            AUC=auc)
    all_auc=c(all_auc,list(dt))
  }
  names(all_auc)=unique(group)
  return(list(PredVal=x,PredClass=pred,ROC=all_auc))
}
