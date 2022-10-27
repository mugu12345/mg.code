getPlotTNMStageCount=function(tnms,gp){
  if(class(gp)!='character'&class(gp)!='factor'){
    gp=paste0('C',gp)
  }
  library(ggplot2)
  all.plots=list()
  all.tabs=list()
  p.ct=0
  for(i in 1:ncol(tnms)){
    if(length(table(tnms[,i]))>1){
      p.ct=p.ct+1
    }
  }
  for(i in 1:ncol(tnms)){
    if(length(table(tnms[,i]))>1){
      tb.count.bs=table(tnms[,i],gp)
      g.tb=matrix(0,nrow=nrow(tb.count.bs),ncol=nrow(tb.count.bs))
      for(i in 1:(nrow(tb.count.bs))){
        for(j in 1:nrow(tb.count.bs)){
          if(i!=j){
            g.tb[i,j]=round(-log10((chisq.test(tb.count.bs[c(i,j),])$p.value)),2)
          }
        }
      }
      colnames(g.tb)=row.names(tb.count.bs)
      row.names(g.tb)=row.names(tb.count.bs)
      g.tb=reshape2::melt(g.tb)
      colnames(g.tb)=c('A1','A2','A3')
      g.labl=g.tb[,3]
      g.labl[is.na(g.labl)]=''
      g.tb$A4=paste0(g.labl,ifelse(!is.na(g.tb[,3])&g.tb[,3]>-log10(0.05),'(*)',''))
      stable.p=ggplot(g.tb, aes(A1, A2)) + geom_tile(aes(fill = A3),colour = "white") +xlab('')+ylab('')+ scale_fill_gradient(low = "white",high = "steelblue")+geom_text(aes(x=A1,y=A2,label=A4))+theme(legend.position="none",axis.title.x=element_blank(),axis.text.x=element_blank())
      tb.count=diag(1/c(apply(tb.count.bs, 1, sum)))%*%tb.count.bs
      row.names(tb.count)=row.names(tb.count.bs)
      mydata=reshape2::melt(tb.count)
      colnames(mydata)=c('C1','C3','C2')
      p=ggplot(mydata,aes(C1,C2,fill=C3))+geom_bar(stat="identity")+ggsci::scale_fill_npg()+theme_bw()+ ylab("percentage")+theme(legend.position = "bottom",legend.title=element_blank(),axis.title.x=element_blank())
      fg=mg_merge_plot(stable.p,p,nrow = 2,ncol = 1,align = "hv",heights = c(0.5,1))
      all.plots=c(all.plots,list(fg))
      #all.tabs=c(all.tabs,list(stable.p))
    }
  }
  p.ct=length(all.plots)
  if(p.ct==4){
    nc=2
  }else if(p.ct%%4==0){
    nc=4
  }else if(p.ct%%3==0){
    nc=3
  }else if(p.ct<=2){
    nc=p.ct
  }else{
    nc=3
  }
  nr=ceiling(p.ct/nc)

  g1=ggpubr::ggarrange(plotlist=all.plots, ncol = nc, nrow = nr,labels=LETTERS[1:length(all.plots)],align = "hv")
  return(g1)
}
