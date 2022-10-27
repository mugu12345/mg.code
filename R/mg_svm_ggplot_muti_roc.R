
mg_svm_ggplot_muti_roc=function(svm_predict_result_list){
  p.dat=rbind()
  for(i in 1:length(svm_predict_result_list)){
    svm_predict_result=svm_predict_result_list[[i]]
    roc_dt=svm_predict_result$ROC[[1]]
    roc.dat=roc(roc_dt$label[!is.na(roc_dt$label)],
                roc_dt$predictions[!is.na(roc_dt$label)])
    fpr=1-roc.dat$specificities
    tpr=roc.dat$sensitivities
    auc=as.numeric(roc.dat$auc)
    los=lowess(fpr, y=tpr, f = 1/3, iter = 100)
    los$x=c(0,los$x,1)
    los$y=c(0,los$y,1)

    if(is.null(names(svm_predict_result_list))){
      lb=paste0(i,':',round(auc,2))
    }else{
      lb=paste0(names(svm_predict_result_list)[i],':',round(auc,2))
    }
    p.dat=rbind(p.dat,data.frame(los$x, y=los$y,rep(lb,length(los$y)),stringsAsFactors = F))
  }
  colnames(p.dat)=c('V1','V2','AUC')
  p.dat=as.data.frame(p.dat)
  library(ggplot2)
  p1=ggplot(p.dat, aes(x=V1,y=V2, fill=AUC))
  p1=p1+geom_line(aes(colour=AUC),lwd=1.1)+theme_bw()+xlab('False positive fraction')+ylab('True positive fraction')
  p1=p1+theme(axis.text.y=element_text(family="Times",face="plain"),axis.text.x=element_text(family="Times",face="plain")
              ,axis.title.x=element_text(family="Times",face="plain"),axis.title.y=element_text(family="Times",face="plain")
              ,plot.title=element_blank()
              ,plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "inches")
              ,legend.position=c(1,0)
              ,legend.justification=c(1,0)
              ,legend.background = element_rect(fill = NA, colour = NA)
              ,legend.title = element_text(family="Times",face="plain")
              ,legend.text = element_text(family="Times",face="plain"))
  return(p1)
}


