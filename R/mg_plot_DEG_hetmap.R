mg_plot_DEG_hetmap=function(heatData,group,logfc,deg_genes,show_colnames=T,show_rownames=T,cluster_cols=T,legend = 'Expression'
                            ,scale = 'row',min_col = 'blue',med_col = 'white',max_col='red'
                            ,max_dt=NULL,med_dt=NULL,min_dt=NULL,cluster_rows=T){
  cm.genes=intersect(deg_genes,row.names(heatData))
  hetDat=heatData[match(cm.genes,row.names(heatData)),]
  fcs=logfc[match(cm.genes,deg_genes)]
  s.ind=which(apply(hetDat, 1, sd)==0)
  if(length(s.ind)>0&scale=='row'){
    fcs=fcs[-s.ind]
    hetDat=hetDat[-s.ind,]
    #scale='none'
  }
  left=mergemHeatmapRowAnno(getmHeatmapAnno(
    name='lfc'
    ,size = 0.3
    ,values = fcs
    ,type = 'x'
    ,bk = c(min(fcs),0,max(fcs))
    ,col = c('green','white','red')
    ,gr = T
  ))
  top=mergemHeatmapColumnAnno(getmHeatmapAnno(
    name='Group'
    ,size = 0.3
    ,values = group
    ,type = 'x'
    ,bk = unique(group)
    ,col = c(mg_colors[1],mg_colors[2])
    ,gr = F
  ))
  pht1=mheatmap((hetDat),min_col =min_col
                ,med_col = med_col
                ,max_col = max_col
                ,max_dt = max_dt
                ,min_dt = min_dt
                ,med_dt = med_dt
                ,annotation_top = top
                ,annotation_left = left
                ,legend = legend
                ,scale = scale
                ,border_color = NA
                ,cluster_rows = cluster_rows
                ,cluster_cols = cluster_cols
                ,show_colnames = show_colnames
                ,show_rownames = show_rownames)
  return(pht1)

}
