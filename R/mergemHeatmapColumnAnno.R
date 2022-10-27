mergemHeatmapColumnAnno=function(...,mlist=NULL){
  ml=list(...)
  if(!is.null(mlist)){
    ml=mlist
  }
  ano.all=NULL
  for(i in 1:length(ml)){
    an1=ml[[i]]
    if(!is.null(an1)){
      #an1=anno11
      anno=NULL
      if(an1$Type=='p'){
        anno=ComplexHeatmap::anno_points(an1$Value,pch=an1$pch,gp = grid::gpar(col = an1$col))
      }else if(an1$Type=='l'){
        anno=ComplexHeatmap::anno_lines(an1$Value,pch=an1$pch,gp = grid::gpar(col = an1$col))
      }else if(an1$Type=='pl'){
        anno=ComplexHeatmap::anno_lines(an1$Value,pch=an1$pch,gp = grid::gpar(col = an1$col),add_points = TRUE,pt_gp = grid::gpar(col = an1$col))
      }else if(an1$Type=='bar'){
        anno=ComplexHeatmap::anno_barplot(an1$Value,bar_width =an1$bar_width,gp = grid::gpar(fill = an1$col))
      }else if(an1$Type=='box'){
        anno=ComplexHeatmap::anno_boxplot(an1$Value,box_width = an1$bar_width,gp = grid::gpar(fill = an1$col))
      }else if(an1$Type=='s'){
        anno=ComplexHeatmap::anno_simple(an1$Value)
      }else if(an1$Type=='x'){
        dtf=data.frame(A=an1$Value)
        colnames(dtf)=c(an1$Name)
        cl=list(A=an1$col)
        names(cl)=c(an1$Name)
        ha=ComplexHeatmap::HeatmapAnnotation(
          df=dtf
          #,name='ABCD'
          ,col=cl
          ,show_annotation_name=T
          ,height=grid::unit(an1$Height,'cm')
          ,simple_anno_size_adjust =T
          ,annotation_legend_param=list(titles=an1$Name
                                        ,title_position='leftcenter-rot')
        )
      }
      if(!is.null(anno)){
        ha <- ComplexHeatmap::HeatmapAnnotation(name='ABCD'
                                                ,A=anno
                                                #,col=an1$col
                                                ,show_annotation_name=T
                                                ,height=grid::unit(an1$Height,'cm')
                                                ,simple_anno_size_adjust =T

        )
      }
      names(ha)=c(an1$Name)
      if(is.null(ano.all)){
        ano.all=ha
      }else{
        ano.all=c(ano.all,ha)
      }
    }
  }
  return(ano.all)
}
