mg_auto_ggplot_roc=function(value,label,group=NULL,col='npg'){
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
  nnm=paste0(ng,' AUC=',round(auc,3))
  longtest$Group=nnm[match(longtest$name,ng)]
  g2=ggplot(longtest, aes(d = D, m = value, color = Group)) + geom_roc(show.legend = TRUE, labels=FALSE) + style_roc()
  g2=g2+mg_get_ggplot_legend('br')
  g2=g2+mg_get_ggsci_col(col,isFill = F)
  return(g2)
}
