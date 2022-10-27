mg_svm_predict=function(model,dat,group){

  pred <- predict (model, dat, probability=TRUE, decision.values=TRUE)
  probs=attr (pred, "probabilities")
  all_auc=list()
  for(i in 1:ncol(probs)){
    prob.versicolor <- probs[,i]
    roc.pred <- ROCR::prediction (prob.versicolor, group == colnames(probs)[i])
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
  names(all_auc)=colnames(probs)[1:ncol(probs)]
  pdt.group=predict (model, dat)
  return(list(predict=pdt.group,ROC=all_auc))
}
