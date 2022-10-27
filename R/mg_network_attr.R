mg_network_attr=function(link,plots=c('A','B','C','D')){
  library(igraph)
  library(ggplot2)
  link1=unique(link[,1:2])
  link1=link1[which(link1[,1]!=link1[,2]),]
  net <- graph_from_data_frame(d = link1,directed = F)
  dge=as.data.frame(table(c(link1[,1],link1[,1])))
  cse=closeness(net)
  bse=betweenness(net)
  ese=evcent(net)$vector

  dge=cbind(dge,cse[match(dge[,1],names(cse))],bse[match(dge[,1],names(bse))],ese[match(dge[,1],names(ese))])
  net_attr=dge[,2:5]
  row.names(net_attr)=dge[,1]
  net_attr=net_attr[order(net_attr[,1],decreasing = T),]
  #net_attr=cbind(degree(net),closeness(net),betweenness(net),evcent(net)$vector)
  colnames(net_attr)=c('Degree','Closeness','Betweenness','Eigenvector')
  #writeMatrix(net_attr,outpath = 'deg.ppi.attr.txt')
  g1=ggplot(as.data.frame(net_attr), aes(x=Degree))+geom_density(color=mg_colors[1], fill="lightblue")
  g1=g1+theme_bw()+theme(axis.text.y=element_text(family="Times",face="plain")
                         #,plot.margin=unit(c(0, 0.2, 0.2, 0.1), "inches")
                         ,plot.title=element_blank()
                         ,legend.position=c(1,1), legend.justification=c(1,1)
                         ,legend.title = element_text(family="Times",face="plain")
                         ,legend.text = element_text(family="Times",face="plain"))
  g2=ggplot(as.data.frame(net_attr), aes(x=Closeness))+geom_density(color=mg_colors[2], fill="lightblue")
  g2=g2+theme_bw()+theme(axis.text.y=element_text(family="Times",face="plain")
                         #,plot.margin=unit(c(0, 0.2, 0.2, 0.1), "inches")
                         ,plot.title=element_blank()
                         ,legend.position=c(1,1), legend.justification=c(1,1)
                         ,legend.title = element_text(family="Times",face="plain")
                         ,legend.text = element_text(family="Times",face="plain"))
  g3=ggplot(as.data.frame(net_attr), aes(x=Betweenness))+geom_density(color=mg_colors[3], fill="lightblue")
  g3=g3+theme_bw()+theme(axis.text.y=element_text(family="Times",face="plain")
                         #,plot.margin=unit(c(0, 0.2, 0.2, 0.1), "inches")
                         ,plot.title=element_blank()
                         ,legend.position=c(1,1), legend.justification=c(1,1)
                         ,legend.title = element_text(family="Times",face="plain")
                         ,legend.text = element_text(family="Times",face="plain"))

  g4=ggplot(as.data.frame(net_attr), aes(x=Eigenvector))+geom_density(color=mg_colors[4], fill="lightblue")
  g4=g4+theme_bw()+theme(axis.text.y=element_text(family="Times",face="plain")
                         #,plot.margin=unit(c(0, 0.2, 0.2, 0.1), "inches")
                         ,plot.title=element_blank()
                         ,legend.position=c(1,1), legend.justification=c(1,1)
                         ,legend.title = element_text(family="Times",face="plain")
                         ,legend.text = element_text(family="Times",face="plain"))
  pt.al=list(A=g1,B=g2,C=g3,D=g4)
  gal=mg_merge_plot(pt.al[match(plots,c('A','B','C','D'))],ncol = length(plots)
                    ,nrow = 1,align = "v",labels = LETTERS[1:length(plots)])
  #gal=ggpubr::ggarrange(g1,g2,g3,g4, ncol = 4, nrow = 1,align = "v",labels = c('A','B','C','D'))

  return(list(plot=gal,table=net_attr))
}

