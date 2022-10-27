plot_GSEA_By_node=function(parseGSEAResult,index=1,TermName=NULL,left=NULL,right=NULL){
  library(ggplot2)
  if(is.null(parseGSEAResult$TEMPLATE)){
    if(is.null(left)){
      left='RankTop'
    }
    if(is.null(right)){
      right='RankBottom'
    }
  }
  if(is.null(left)){
    left=parseGSEAResult$TEMPLATE[1]
  }
  if(is.null(right)){
    right=parseGSEAResult$TEMPLATE[2]
  }
  if(!is.null(TermName)){
    ind=which(parseGSEAResult$EnrichTable[,1]==TermName)
    if(length(ind)==0){
      print(paste0(TermName,' Not Found!'))
      return(NA)
    }else{
      ind=ind[1]
    }
  }else{
    ind=index
    if(ind>nrow(parseGSEAResult$EnrichTable)){
      print(paste0(ind,' out range!'))
      return(NA)
    }
  }
  node=parseGSEAResult$Nodes[[ind]]
  g.rnk=parseGSEAResult$Rank
  es.values=c(0,as.numeric(unlist(strsplit(XML::xmlGetAttr(node,'ES_PROFILE'),' '))),0)
  hit.index=c(0,as.numeric(unlist(strsplit(XML::xmlGetAttr(node,'HIT_INDICES'),' '))),nrow(g.rnk))
  es=as.numeric(XML::xmlGetAttr(node,'ES'))
  nes=as.numeric(XML::xmlGetAttr(node,'NES'))
  np=as.numeric(XML::xmlGetAttr(node,'NP'))
  FDR=as.numeric(XML::xmlGetAttr(node,'FDR'))

  dat=data.frame(Index=hit.index,ES=es.values)
  p=ggplot(data=dat, aes(x=Index, y=ES)) +geom_line(aes(colour='darkgreen',size=2))+xlim(0,nrow(g.rnk))+theme_bw()+ylab('Enrichment score')+labs(title=gsub('^gene_sets.gmt#','',XML::xmlGetAttr(node,'GENESET')))+theme(plot.title = element_text(hjust = 0.5))
  p=p+ geom_segment(aes(x = 0, xend = nrow(g.rnk), y = 0, yend = 0)
                    , color="grey"
                    ,linetype="dashed")

  #p+geom_text(data=data.frame(Xl=c(0),yl=c(min(es.values))),aes(x=0,y=min(es.values),label = paste0('ES=',signif(es,2),'\nNES=',signif(nes,2)
  #                                                              ,'\nP=',signif(np,2),'\nFDR=',signif(FDR,2)))
  #            ,vjust =0, hjust = 0)

  if(es<0){
    p=p+geom_text(data=data.frame(Xl=c(0),yl=c(min(es.values))),aes(x=0,y=min(es.values),label = paste0('ES=',signif(es,2),'\nNES=',signif(nes,2),'\nP=',signif(np,2),'\nFDR=',signif(FDR,2)))
                  ,vjust =0, hjust = 0)
  }else{
    p=p+geom_text(data=data.frame(Xl=c(0),yl=c(min(es.values))),aes(x=nrow(g.rnk),y=max(es.values),label = paste0('ES=',signif(es,2),'\nNES=',signif(nes,2),'\nP=',signif(np,2),'\nFDR=',signif(FDR,2)))
                  ,vjust =1, hjust = 1)
  }
  es.min=min(dat$ES)
  if(es.min>0){
    es.min=0
  }

  ymin=es.min-(max(dat$ES)-es.min)*0.1
  dt1=dat
  dt2=dat
  dt1$Height=rep(ymin,nrow(dat))
  dt2$Height=rep(ymin+(es.min-ymin)*0.7,nrow(dat))
  p1=p+geom_line(data = rbind(dt1,dt2),aes(x=Index,y=Height,group=Index))
  p1=p1+ggforce::geom_link(data=data.frame(x=c(0,nrow(g.rnk)),y=c(ymin,ymin),xend=c(nrow(g.rnk)/2,nrow(g.rnk)/2),yend=c(ymin,ymin))
                           ,aes(x=x,y=y,xend=xend,yend=yend
                                ,alpha=1-..index..
                                ,colour=c('red','blue')
                                ,size=50
                           ))
  p1=p1+theme(legend.position='none',axis.title.x=element_blank(),axis.text.x = element_blank())
  p1=p1+geom_text(data=data.frame(Xl=c(0),yl=c(ymin)),
                  aes(x=0,y=ymin,label = left),vjust =0.5, hjust = 0)+geom_text(data=data.frame(Xl=c(nrow(g.rnk)),yl=c(ymin)),
                                                                                aes(x=nrow(g.rnk),y=ymin,label = right),vjust =0.5, hjust = 1)
  p1=p1+theme(plot.margin=unit(c(0.2, 0.2, 0, 0.1), "inches"))

  p2=ggplot(data=data.frame(Risk=c(0,g.rnk$V2,0),Index=c(1,1:nrow(g.rnk),nrow(g.rnk))),aes(y=Risk,x=Index))+geom_line()+theme_bw()
  p2=p2+ geom_segment(aes(x = 0, xend = nrow(g.rnk), y = 0, yend = 0)
                      , color="grey"
                      ,linetype="dashed")
  p2=p2+theme(plot.margin=unit(c(0, 0.2, 0.2, 0.1), "inches"))+ylab('Rank')+xlab('Rank in ordered dataset')
  gal=ggpubr::ggarrange(p1,p2, ncol = 1, nrow = 2,heights = c(1,0.6),align = "v")
  return(gal)
}
