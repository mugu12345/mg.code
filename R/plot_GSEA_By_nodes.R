plot_GSEA_By_nodes=function(parseGSEAResult,indexs=c(1,2,3),TermNames=NULL,left=NULL,right=NULL){
  #parseGSEAResult=gsea.result.kegg.result
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
  if(!is.null(TermNames)){
    inds=match(TermNames,parseGSEAResult$EnrichTable[,1])
    inds=inds[!is.na(inds)]
    if(length(inds)==0){
      print(paste0(TermNames,' Not Found!'))
      return(NA)
    }
  }else{
    inds=indexs
    if(max(inds)>nrow(parseGSEAResult$EnrichTable)){
      print(paste0(inds,' out range!'))
      return(NA)
    }
  }
  #parseGSEAResult=GSE17705_GSEA
  g.rnk=parseGSEAResult$Rank

  all.info=rbind()
  all.dat=rbind()
  for(i in inds){
    node=parseGSEAResult$Nodes[[i]]
    es.values=c(0,as.numeric(unlist(strsplit(XML::xmlGetAttr(node,'ES_PROFILE'),' '))),0)
    hit.index=c(0,as.numeric(unlist(strsplit(XML::xmlGetAttr(node,'HIT_INDICES'),' '))),nrow(g.rnk))
    m.inds=which.max(abs(es.values))
    es=as.numeric(XML::xmlGetAttr(node,'ES'))
    np=as.numeric(XML::xmlGetAttr(node,'NP'))
    FDR=as.numeric(XML::xmlGetAttr(node,'FDR'))
    nes=as.numeric(XML::xmlGetAttr(node,'NES'))
    title=gsub('^gene_sets.gmt#','',XML::xmlGetAttr(node,'GENESET'))
    length(hit.index)
    all.dat=rbind(all.dat,data.frame(Index=hit.index,ES=es.values,Term=rep(title,length(es.values))
                                     ,Type=c('A',rep('V',length(es.values)-2),'A')))
    all.info=rbind(all.info,c(title,es.values[m.inds[1]],
                              hit.index[m.inds[1]],es,nes,np,FDR))
  }
  all.info=crbind2DataFrame(all.info)
  #all.info

  cbPalette=gsub('99$','',c(ggsci::pal_npg("nrc", alpha = 0.6)(10)
                            ,ggsci::pal_d3("category20", alpha = 0.6)(20)
                            ,ggsci::pal_nejm("default", alpha = 0.6)(8)))

  all.dat$Colour=cbPalette[as.numeric(as.factor(all.dat$Term))]

  col_mp=unique(cbind(as.character(all.dat$Term),all.dat$Colour))[,2]
  names(col_mp)=unique(cbind(as.character(all.dat$Term),all.dat$Colour))[,1]
  glb=unique(unlist(lapply(strsplit(all.info[,1],'_'),function(x){return(x[1])})))
  if(length(glb)==1){
    #g.labels=paste0(gsub(paste0('^',glb,'_'),'',g.labels))
    desc=gsub(paste0('^',glb,'_'),'',all.info[,1])
  }else{
    desc=all.info[,1]
  }
  ndesc=c()
  for(de in desc){
    #de=desc[1]
    if(nchar(de)>50){
      d2=paste0(substr(de,0,47),'...')
      ndesc=c(ndesc,d2)
    }else{
      ndesc=c(ndesc,de)
    }
  }
  g.labels=paste0(ndesc,'\nES=',signif(all.info[,4],2),',NES=',signif(all.info[,5],2),',P=',signif(all.info[,6],2),',FDR=',signif(all.info[,7],2))[match(names(col_mp),all.info[,1])]

  #dat=data.frame(Index=hit.index,ES=es.values)
  p=ggplot(data=all.dat, aes(x=Index, y=ES)) +geom_line(aes(colour=Term))+xlim(0,nrow(g.rnk))
  if(length(glb)==1){
    p=p+labs(title=paste0('Enrichment plot ',glb,' terms'))+theme(plot.title = element_text(hjust = 0.5))
  }
  p=p+scale_colour_manual(values=col_mp
                          ,breaks = names(col_mp)
                          ,labels = g.labels
  )
  p=p+ylab('Enrichment score')+theme_bw()
  #p+guides(color = FALSE)
  p=p+ geom_segment(aes(x = 0, xend = nrow(g.rnk), y = 0, yend = 0)
                    , color="grey"
                    ,linetype="dashed")

  p=p+theme(
    legend.position='none'
    ,axis.title.x=element_blank()
    ,axis.text.x = element_blank(),axis.ticks.x = element_blank()
    ,plot.margin=unit(c(0.2, 0.2, 0, 0.1), "inches"))
  #p+geom_label()
  es.min=min(all.dat$ES)
  ymin=es.min-(max(all.dat$ES)-es.min)*0.1

  lh=(es.min-ymin)*0.7

  dt1=all.dat
  dt2=all.dat
  dt1$Height=rep(ymin,nrow(all.dat))-lh*(as.numeric(as.factor(all.dat$Term))-1)
  dt2$Height=rep(ymin+(es.min-ymin)*0.7,nrow(all.dat))-lh*(as.numeric(as.factor(all.dat$Term))-1)
  dt1=dt1[which(dt1$Type!='A'),]
  dt2=dt2[which(dt2$Type!='A'),]
  #dt=rbind(dt1)
  p1=ggplot()
  #es.text=rbind()
  for(g in unique(dt1$Term)){
    cl=unique(dt1$Colour[which(dt1$Term==g)])[1]
    p1=p1+geom_line(data = rbind(dt1[which(dt1$Term==g),],dt2[which(dt1$Term==g),])
                    ,aes(x=Index,y=Height,group=Index),col=cl)
    #h1=dt1$Height[which(dt1$Term==g)][1]
    #es.text=rbind(es.text,c(g,0,h1))
  }
  #es.text=crbind2DataFrame(es.text)
  #all.info$SX=es.text[match(all.info[,1],es.text[,1]),2]
  #all.info$SY=es.text[match(all.info[,1],es.text[,1]),3]

  #p1=p1+geom_text(data = all.info,aes(x=SX,y=SY,label = paste0('ES=',signif(V4,2),',NES=',signif(V5,2),',P=',signif(V6,2),',FDR=',signif(V7,2)))
  #              ,vjust =-0, hjust = 0)

  p1=p1+theme_bw()+theme(legend.position='none',axis.title.y=element_blank(),axis.text.y = element_blank()
                         ,axis.title.x=element_blank(),axis.text.x = element_blank()
                         ,axis.line.x = element_blank(),axis.ticks.x = element_blank(),axis.ticks.y = element_blank()
                         ,axis.line.x.bottom = element_blank()
                         ,plot.margin=unit(c(0, 0.2, 0, 0.1), "inches")
                         ,axis.line = element_blank()
  )

  #ggpubr::ggarrange(p,p1, ncol = 1, nrow = 2,heights = c(1,0.1*length(inds)),align = "v")

  p2=ggplot(data=data.frame(Risk=c(0,g.rnk$V2,0),Index=c(1,1:nrow(g.rnk),nrow(g.rnk))),aes(y=Risk,x=Index))+geom_line()+theme_bw()
  p2=p2+ geom_segment(aes(x = 0, xend = nrow(g.rnk), y = 0, yend = 0)
                      , color="grey"
                      ,linetype="dashed")
  p2=p2+theme(plot.margin=unit(c(0, 0.2, 0.2, 0.1), "inches"))+ylab('Rank')+xlab('Rank in ordered dataset')
  p2=p2+geom_text(data=data.frame(Xl=c(0),Yl=c(0)),aes(x=0,y=0,label = left),vjust =1, hjust = 0)+geom_text(data=data.frame(Xl=c(nrow(g.rnk)),Yl=c(0)),
                                                                                                            aes(x=nrow(g.rnk),y=0,label = right),vjust =0, hjust = 1)
  g.h=0.1*length(inds)
  if(g.h>0.8){
    g.h=0.8
  }
  gal=ggpubr::ggarrange(p,p1,p2, ncol = 1, nrow = 3,heights = c(1,g.h,0.6),align = "v",common.legend = TRUE,legend = "right")
  return(gal)
}
