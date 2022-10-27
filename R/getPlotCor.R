getPlotCor=function(dat,method='pearson',filter=T){
  library(ggplot2)
  tdt=dat
  olds=colnames(tdt)
  colnames(tdt)=c('C1','C2')
  u1=mean(tdt[,1])+3*sd(tdt[,1])
  u2=mean(tdt[,2])+3*sd(tdt[,2])
  if(filter){
    tdt=tdt[tdt[,1]<u1&tdt[,2]<u2,]
  }
  rho2=cor.test(tdt[,1],tdt[,2],method = method)
  p1=ggplot(tdt,aes(x=C1,y=C2))+geom_point()+geom_smooth(method = 'lm')+xlab(olds[1])+ylab(olds[2])
  p1=p1+ggtitle(paste0(unlist(strsplit(rho2$method,' '))[1],'(R=',round(rho2$estimate,2),',P=',signif(rho2$p.value,2),')'))
  p1=p1+ggsci::scale_fill_npg()+theme_bw()+theme(legend.position = "none",legend.title=element_blank(),plot.title = element_text(hjust = 0.5))
  #detach('package:ggplot2')
  return(p1)
}
