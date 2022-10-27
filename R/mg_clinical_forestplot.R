mg_clinical_forestplot=function(clinical_riskscore,os,event,is_muti_cox=T,show_95CI=T,zero = 1,boxsize = 0.4,lineheight =5,colgap =2,lwd.zero=2,lwd.ci=2
                                ,box_col='#458B00',summary_col="#8B008B",lines_col='black',zero_col='#7AC5CD'
                                ,xlab='HR',lwd.xaxis=2,lty.ci = "solid",graph.pos = 2,xlim=NULL,xlog=F){
  #clinical_riskscore=geo.clini2.tnm
  #os=geo.data2$A1_OS
  #event=geo.data2$Status
  sig.cox=t(apply(clinical_riskscore, 2,function(x){
    x1=as.numeric(as.character(x))
    if(sum(is.na(x1))>length(x1)*0.5){
      x1=as.numeric(as.factor(x))
    }
    return(coxRun(dat = data.frame(os,event,x1)))
  }))
  colnames(sig.cox)=c('p.value','HR','95%CI_upper','95%CI_lower')
  sig.cox=sig.cox[which(!is.na(sig.cox[,1])),]
  sig.cox1=cbind(pvalue=round(sig.cox[,1],3),round(sig.cox[,2:4],2))

  clinical_riskscore1=clinical_riskscore[,match(row.names(sig.cox),colnames(clinical_riskscore))]
  dat=(apply(clinical_riskscore1, 2,function(x){
    x1=as.numeric(as.character(x))
    if(sum(is.na(x1))>length(x1)*0.5){
      x1=as.numeric(as.factor(x))
    }
    return(x1)
  }))
  muti.cox=createCoxModel(dat = dat ,time = os,event = event)
  muti.sig=cbind(pvalue=round(summary(muti.cox$Cox)[[7]][,5],3),round(summary(muti.cox$Cox)[[8]][,c(1,3,4)],2))
  row.names(muti.sig)=muti.cox$Genes
  #lay2 <- customLayout::lay_new(matrix(1:1))
  #lay1 <- customLayout::lay_new(matrix(1:1))
  #cl <- customLayout::lay_bind_col(lay2, lay1, widths = c(1, 1))
  #customLayout::lay_show(cl)
  #customLayout::lay_set(cl)
  #p1=mg_forestplot_v2(cbind(row.names(muti.sig),muti.sig))
  #par(mfrow=c(1,2))
  #mg_forestplot_v2(cbind(row.names(muti.sig),muti.sig))
  #mg_forestplot_v2(cbind(row.names(sig.cox1),sig.cox1))
  #print(p1)
  if(is_muti_cox){
    mg_forestplot_v2(cbind(row.names(muti.sig),muti.sig),show_95CI=show_95CI,zero = zero,boxsize =boxsize,lineheight =lineheight,colgap =colgap
                     ,lwd.zero=lwd.zero,lwd.ci=lwd.ci
                     ,box_col=box_col,summary_col=summary_col,lines_col=lines_col,zero_col=zero_col
                     ,xlab=xlab,lwd.xaxis=lwd.xaxis,lty.ci = lty.ci,graph.pos = graph.pos,xlim=xlim,xlog=xlog)
  }else{
    mg_forestplot_v2(cbind(row.names(sig.cox1),sig.cox1),show_95CI=show_95CI,zero = zero,boxsize =boxsize,lineheight =lineheight,colgap =colgap
                     ,lwd.zero=lwd.zero,lwd.ci=lwd.ci
                     ,box_col=box_col,summary_col=summary_col,lines_col=lines_col,zero_col=zero_col
                     ,xlab=xlab,lwd.xaxis=lwd.xaxis,lty.ci = lty.ci,graph.pos = graph.pos,xlim=xlim,xlog=xlog)
  }
  return(list(one=sig.cox,muti=muti.sig))
}
