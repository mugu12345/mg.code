mg_auto_cal_AUC=function(value,label,group=NULL){
  library(plotROC)
  library(ggplot2)
  if(is.null(group)){
    group=rep('AUC',length(value))
  }else if(length(value)!=length(group)){
    group=rep(group,length(value))[1:length(value)]
  }
  longtest=crbind2DataFrame(data.frame(value,label,group))
  colnames(longtest)=c('value','D','name')
  g1=ggplot(longtest, aes(d = D, m = value, color = name)) + geom_roc() + style_roc()
  ng=levels(factor(longtest$name))
  auc <- calc_auc(g1)$AUC
  return(crbind2DataFrame(cbind(Label=ng,AUC=auc)))
}
