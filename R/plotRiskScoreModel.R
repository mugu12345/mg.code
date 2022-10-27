plotRiskScoreModel=function(riskScore,dat,time,event,cutoff,hetTitle='z-score of expression',hetColor=c('green','black','red')){
  srt.inds=order(riskScore)
  dat=dat[srt.inds,]
  time=time[srt.inds]
  event=event[srt.inds]
  riskScore=riskScore[srt.inds]
  library(ggplot2)
  dt1=data.frame(V1=1:length(riskScore),V2=riskScore,RiskType=ifelse(riskScore>cutoff,'High','Low'))
  p1=ggplot(dt1, aes(x = V1, y = V2, colour = RiskType,fill=RiskType)) +geom_bar(stat = 'identity', position = 'dodge')+ggsci::scale_fill_npg()+theme_bw()
  p1=p1+ylab('RiskScore')+theme(axis.text.y=element_text(family="Times",face="plain"),axis.text.x=element_blank()
                                ,axis.title.x=element_blank(),legend.position=c(1,0), legend.justification=c(1,0)
                                ,legend.background = element_rect(fill = NA, colour = NA)
                                ,plot.margin=unit(c(0.1, 0.1, 0, 0.1), "inches")
                                ,legend.title = element_text(family="Times",face="plain")
                                ,legend.text = element_text(family="Times",face="plain"))

  dt2=data.frame(V1=1:length(riskScore),V2=time,Status=ifelse(event==1,'Dead','Alive'))
  p2=ggplot(dt2, aes(x = V1, y = V2, colour = Status,shape =Status)) +geom_point()+ggsci::scale_fill_npg()+theme_bw()
  p2=p2+ylab('Time')+theme(axis.text.y=element_text(family="Times",face="plain"),axis.text.x=element_blank()
                           ,axis.title.x=element_blank(),legend.position=c(1,1), legend.justification=c(1,1)
                           ,legend.background = element_rect(fill = NA, colour = NA)
                           ,plot.margin=unit(c(0, 0.1, 0, 0.1), "inches")
                           ,legend.title = element_text(family="Times",face="plain")
                           ,legend.text = element_text(family="Times",face="plain"))

  data=as.data.frame(scale(dat))
  hc.r = hclust(dist(t(data)))
  data=data[,hc.r$order]
  data$ID <- 1:nrow(dat)
  #colnames(data)
  data_m <- reshape2::melt(data, id.vars=c("ID"))
  colnames(data_m)=c('ID','V1','V2')
  data_m$V2[which(data_m$V2>mean(data_m$V2)+3*sd(data_m$V2))]=mean(data_m$V2)+3*sd(data_m$V2)
  data_m$V2[which(data_m$V2<mean(data_m$V2)-3*sd(data_m$V2))]=mean(data_m$V2)-3*sd(data_m$V2)

  data_m$V1=mg_str_outline(data_m$V1,isCut = T,n=50)
  #print(data_m$V1)
  #print(head(data_m))
  #data_m[1:20,]
  p3 <- ggplot(data_m, aes(x=ID,y=V1))
  p3 <- p3 + geom_tile(aes(fill=V2))
  p3=p3+scale_fill_gradient2(low = hetColor[1],mid=hetColor[2], high = hetColor[3])
  p3=p3+theme_bw()
  p3=p3+ labs(fill=hetTitle)
  #p3=p3+guides(fill=guide_legend(title="New Legend Title"))
  p3=p3+xlab('Samples')
  #p3 <- p3 + theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
  p3=p3+theme(axis.text.y=element_text(family="Times",face="plain")
              ,axis.text.x=element_blank()
              #,axis.title.x=element_blank()
              ,axis.title.y=element_blank()
              ,legend.position='bottom'
              #,legend.justification=c(1,1)
              #,legend.background = element_rect(fill = NA, colour = NA)
              ,plot.margin=unit(c(0, 0.1, 0.1, 0.1), "inches")
              ,legend.title = element_text(family="Times",face="plain")
              ,legend.text = element_text(family="Times",face="plain"))

  g1=ggpubr::ggarrange(p1,p2,p3, ncol = 1, nrow = 3,heights = c(0.5,0.5,1),align = "v")
  return(g1)
}
