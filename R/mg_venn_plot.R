mg_venn_plot=function(data_list,fill=NULL,ven=T){
  if(length(data_list)<=4&ven){
    library(VennDiagram)
    if(is.null(fill)){
      fill=c('red', 'blue','green','yellow')[1:length(data_list)]
    }
    g=venn.diagram(data_list, filename = NULL,margin = 0.2,
                   fill = fill, alpha = 0.50, col = 'black', cex = 1, fontfamily = 'serif'
                   #,cat.col = c('black', 'black', 'black', 'black')
                   ,cat.cex = 1, cat.fontfamily = 'serif')
    grid.draw(g)
  }else if(length(data_list)<=6&ven){
    library(venn)
    if(is.null(fill)){
      fill=mg_colors[1:length(data_list)]
    }
    g=venn(data_list, zcolor = fill,ggplot=F,box=F)
  }else{
    anm=unique(unlist(data_list))
    tbs=cbind()
    for(i in 1:length(data_list)){
      a1=rep(0,length(anm))
      a1[anm%in%data_list[[i]]]=1
      tbs=cbind(tbs,a1)
    }
    colnames(tbs)=names(data_list)
    row.names(tbs)=anm
    tbs=crbind2DataFrame(tbs)
    g=UpSetR::upset(tbs, nsets = ncol(tbs), nintersects = 30, mb.ratio = c(0.5, 0.5),
                    order.by = c("freq", "degree"), decreasing = c(TRUE,FALSE)
    )
    return(g)
  }
}
