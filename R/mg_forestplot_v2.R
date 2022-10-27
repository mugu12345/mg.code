mg_forestplot_v2=function(dat,show_95CI=T,zero = 1,boxsize = 0.4,lineheight =5,colgap =2,lwd.zero=2,lwd.ci=2
                          ,box_col='#458B00',summary_col="#8B008B",lines_col='black',zero_col='#7AC5CD'
                          ,xlab='HR',lwd.xaxis=2,lty.ci = "solid",graph.pos = 2,xlim=NULL,xlog=F){
  nc=ncol(dat)
  nr=nrow(dat)
  library(forestplot)
  col=fpColors(box=box_col,summary=summary_col,lines = lines_col,zero = zero_col)

  if(nc>3){
    hr=as.numeric(dat[,nc-2])
    lower=as.numeric(dat[,nc-1])
    upper=as.numeric(dat[,nc])

    if(is.null(xlim)){
      xlim=c(min(lower,na.rm = T),max(upper,na.rm = T))
    }
    if(is.infinite(max(xlim))){
      xlim=c(xlim[1],5)
    }
    if(is.infinite(min(xlim))){
      xlim=c(0,xlim[2])
    }

    if(min(xlim)<=0){
      xlog=F
    }
    smary=rep(F,length(hr))
    nind=which(is.na(lower)|is.na(upper)|is.na(hr))
    smary[nind]=T
    labeltext=as.matrix(dat[,1:(nc-3)])
    if(show_95CI){
      adt=paste0(round(hr,2),'(',round(lower,2),',',round(upper,2),')')
      adt[nind]=''
      labeltext=cbind(labeltext,adt)
      colnames(labeltext)=c(colnames(labeltext)[1:(ncol(labeltext)-1)],'Hazard Ratio(95% CI)')
    }
    if(graph.pos>ncol(labeltext)+1){
      labeltext=ncol(labeltext)+1
    }else if(graph.pos<2){
      graph.pos=2
    }
    hz_list=list('2'=gpar(lty=1,col=summary_col),
                 '3'=gpar(lty=1,col=summary_col)
    )
    names(hz_list)=c(2,nrow(labeltext)+2)
    p=forestplot(labeltext = rbind(colnames(labeltext),labeltext),
                 hrzl_lines = hz_list,
                 mean = c(NA,hr),
                 lower =c(NA,lower),
                 upper = c(NA,upper),
                 is.summary=c(T,smary),
                 zero = zero,
                 boxsize = boxsize,
                 lineheight = unit(lineheight,'mm'),
                 colgap = unit(colgap,'mm'),
                 lwd.zero = lwd.zero,
                 lwd.ci = lwd.ci,
                 col=col,
                 xlab=xlab,
                 lwd.xaxis=lwd.xaxis,
                 lty.ci = lty.ci,
                 clip = xlim,
                 xlog=xlog,
                 graph.pos = graph.pos)
    return(p)
  }else{
    return(mg_getplot_bank('data must be greater than 3 column'))
  }
}
