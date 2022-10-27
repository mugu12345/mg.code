mergemHeatmapRowAnno=function(...,mlist=NULL){
  ml=list(...)
  if(!is.null(mlist)){
    ml=mlist
  }
  #if(length(ml))
  ano.all=NULL
  for(i in 1:length(ml)){
    an1=ml[[i]]
    if(!is.null(an1)){
      anno=NULL
      #print(i)
      #print(an1)
      #print('=======')
      if(an1$Type=='p'){
        anno=ComplexHeatmap::row_anno_points(an1$Value,pch=an1$pch,gp = grid::gpar(col = an1$col))
      }else if(an1$Type=='bar'){
        anno=ComplexHeatmap::row_anno_barplot(an1$Value,bar_width =an1$bar_width,gp = grid::gpar(fill = an1$col))
      }else if(an1$Type=='box'){
        anno=ComplexHeatmap::row_anno_boxplot(an1$Value,bar_width =an1$bar_width,gp = grid::gpar(fill = an1$col))
      }else if(an1$Type=='x'){
        dtf=data.frame(A=an1$Value)
        colnames(dtf)=c(an1$Name)
        cl=list(A=an1$col)
        names(cl)=c(an1$Name)
        ha=ComplexHeatmap::rowAnnotation(
          df=dtf
          #,name='ABCD'
          ,col=cl
          ,show_annotation_name=T
          ,width=unit(an1$Height,'cm')
          ,simple_anno_size_adjust =T
          ,annotation_legend_param=list(titles=an1$Name
                                        ,title_position='leftcenter-rot')
        )
      }
      if(!is.null(anno)){
        ha <- ComplexHeatmap::rowAnnotation(name='ABCD'
                                            ,A=anno
                                            #,col=an1$col
                                            ,show_annotation_name=T
                                            ,width=unit(an1$Height,'cm')
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
