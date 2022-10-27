mheatmap=function(mat
                  ,max_col='red'
                  ,min_col='green'
                  ,med_col='white'
                  ,max_dt=NULL
                  ,min_dt=NULL
                  ,med_dt=NULL
                  ,color = NULL
                  ,legend='exp', na_col = "#DDDDDD"
                  ,kmeans_k = NA
                  ,border_color = "grey60"
                  ,scale = "none", cluster_rows = TRUE,cluster_cols = TRUE
                  ,clustering_distance_rows = "euclidean",clustering_distance_cols = "euclidean",clustering_method = "complete"
                  ,cutree_rows = NA, cutree_cols = NA
                  ,show_rownames = T, show_colnames = T,fontsize = 10, fontsize_row = NULL, fontsize_col = NULL,column_names_rot=90,row_names_rot=0
                  ,labels_row = NULL,labels_col = NULL
                  ,row_title=character(0),column_title=character(0)
                  ,row_names_side='right',column_names_side = 'bottom'
                  ,row_dend_side='left',column_dend_side = 'top'
                  ,display_numbers = F,number_format = "%.2f", number_color = "grey30",fontsize_number = 10
                  ,annotation_top=NULL
                  ,annotation_right=NULL
                  ,annotation_left=NULL
                  ,annotation_bottom=NULL
                  ,column_split=NULL
                  ,row_split=NULL
){



  #,cellwidth = NA,cellheight = NA
  #, breaks = NA, border_color = "grey60"
  #,clustering_callback = identity2
  #,treeheight_row = ifelse((class(cluster_rows) == "hclust") || cluster_rows, cluster_cols, 50, 0)
  #,legend = TRUE, legend_breaks = NA,legend_labels = NA
  #,annotation_row = NA, annotation_col = NA
  #,annotation = NA, annotation_colors = NA, annotation_legend = TRUE,annotation_names_row = TRUE
  #,annotation_names_col = TRUE,drop_levels = TRUE, main = NA
  #,angle_col = c("270", "0", "45", "90", "315")
  #,display_numbers = F,number_format = "%.2f", number_color = "grey30"
  #,fontsize_number = 0.8 * fontsize
  #,gaps_row = NULL, gaps_col = NULL, filename = NA
  #,width = NA, height = NA,silent = FALSE

  #color = circlize::colorRamp2(c(-1, 0, 1), c('green', 'white', 'red'))
  #legend='exp'
  #kmeans_k=NA
  #clustering_distance_rows='euclidean'
  #clustering_distance_cols='euclidean'
  #clustering_method = "complete"
  #na_col = "#DDDDDD"
  #cluster_rows=F
  #cluster_cols=T
  #cutree_rows=2
  #cutree_cols=3
  #show_rownames = T
  #show_colnames = T
  #labels_row = NULL
  #labels_col = NULL
  #row_dend_side='left'
  #column_dend_side = 'top'
  #row_title='AA'
  #column_title=character(0)
  #row_names_side='right'
  #column_names_side = 'bottom'

  #display_numbers=T
  cell_fun=NULL
  #number_format = "%.2f"
  #number_color = "grey30"
  #border_color='black'
  #fontsize_number = 8
  #fontsize = 10
  #fontsize_row=NULL
  #fontsize_col=NULL
  #column_names_rot=0
  #row_names_rot=0


  if(is.null(fontsize_row)){
    fontsize_row = fontsize
  }
  if(is.null(fontsize_col)){
    fontsize_col = fontsize
  }
  if(is.null(labels_col)){
    labels_col=colnames(mat)
  }
  if(is.null(labels_row)){
    labels_row=row.names(mat)
  }
  if(display_numbers){
    cell_fun = function(j, i, x, y, width, height, fill) {
      grid::grid.text(sprintf(number_format, mat[i, j]), x, y, gp = grid::gpar(fontsize = fontsize_number,col=number_color))
      if(!is.null(border_color)){
        grid::grid.rect(x = x, y = y, width = width, height = height, gp = grid::gpar(col = border_color, fill = NA))
      }
    }
  }else{
    cell_fun = function(j, i, x, y, width, height, fill) {
      if(!is.null(border_color)){
        grid::grid.rect(x = x, y = y, width = width, height = height, gp = grid::gpar(col = border_color, fill = NA))
      }
    }
  }
  if(scale=='row'){
    mat=as.data.frame(t(scale(t(mat),center = T)))
  }else if(scale=='column'){
    mat=as.data.frame(scale(mat,center = T))
  }

  if(is.null(color)){
    if(is.null(max_dt)){
      max_dt=max(mat,na.rm = T)
    }
    if(is.null(min_dt)){
      min_dt=min(mat,na.rm = T)
    }
    if(min_dt>=max_dt){
      min_dt=min(mat,na.rm = T)
      max_dt=max(mat,na.rm = T)
      med_dt=(min_dt+max_dt)/2
      #med_dt=median(matrix2vator(mat),na.rm = T)
    }
    if(is.null(med_dt)){
      med_dt=(min_dt+max_dt)/2
    }
    if(med_dt>=max_dt|med_dt<=min_dt){
      med_dt=(min_dt+max_dt)/2
    }
    if(is.null(med_col)){
      color=circlize::colorRamp2(c(min_dt, max_dt), c(min_col, max_col))
    }else{
      color=circlize::colorRamp2(c(min_dt, med_dt, max_dt), c(min_col, med_col, max_col))
    }

  }

  if(!is.na(cutree_cols)&cluster_cols){
    cl1=ComplexHeatmap::HeatmapAnnotation(foo = ComplexHeatmap::anno_block(gp = gpar(fill = 2:(cutree_cols+1)),
                                                                           labels = paste0("cluster",1:cutree_cols)
                                                                           , labels_gp = gpar(col = "white")))

    if(is.null(annotation_top)){
      annotation_top=cl1
    }else{
      annotation_top=c(cl1,annotation_top)
    }
  }
  if(!is.na(cutree_cols)&cluster_rows){
    rl1=ComplexHeatmap::rowAnnotation(foo = ComplexHeatmap::anno_block(gp = gpar(fill = 2:(cutree_rows+1)),
                                                                       labels = paste0("cluster",1:cutree_rows)
                                                                       , labels_gp = gpar(col = "white")))

    if(is.null(annotation_left)){
      annotation_left=rl1
    }else{
      annotation_left=c(cl1,annotation_left)
    }
  }
  pl=ComplexHeatmap::Heatmap(mat,col = color,name = legend,column_km=ifelse(is.na(cutree_cols),1,cutree_cols)
                             ,km=ifelse(is.na(cutree_rows),1,cutree_rows)
                             ,cluster_rows =cluster_rows,cluster_columns =cluster_cols
                             ,clustering_distance_rows = clustering_distance_rows
                             ,clustering_distance_columns=clustering_distance_cols
                             ,clustering_method_columns=clustering_method
                             ,na_col=na_col
                             ,show_row_names=show_rownames
                             ,show_column_names = show_colnames
                             ,row_labels =labels_row
                             ,column_labels = labels_col
                             ,row_title=row_title,column_title=column_title
                             ,row_dend_side=row_dend_side
                             ,row_names_side=row_names_side
                             ,column_names_side = column_names_side
                             ,column_dend_side = column_dend_side
                             ,cell_fun=cell_fun
                             ,column_names_gp = grid::gpar(fontsize = fontsize_col)
                             ,row_names_gp = grid::gpar(fontsize = fontsize_row)
                             ,column_names_rot=column_names_rot
                             ,row_names_rot = row_names_rot
                             ,column_names_centered=ifelse(column_names_rot==0,T,F)
                             #,gap=grid::unit(10, "mm")
                             ,heatmap_legend_param = list(
                               title = legend,
                               title_position = "leftcenter-rot"
                             )
                             ,top_annotation = annotation_top
                             ,left_annotation = annotation_left
                             ,right_annotation = annotation_right
                             ,bottom_annotation = annotation_bottom
                             ,column_split = column_split
                             ,row_split = row_split
  )
  return(pl)
}
