mg_plot_hetmap_cor=function(split1,spli2=NULL,split1_range=NULL,split1_range_cols=NULL,show_text1=NULL,leg_title1='Panel_top',leg_title2='Panel_bottom'
                            ,split2_range=NULL,split2_range_cols=NULL,show_text2=NULL
                            ,title=NULL
                            ,type=c('pie','circle','color')[1],cell_width=NULL){
  if(is.null(cell_width)){
    cell_width=2
  }
  cell_height=cell_width

  library(grid)
  library(ComplexHeatmap)
  up=split1
  down=spli2
  if(is.null(down)){
    down=up
  }
  if(is.null(split1_range)|length(split1_range)==1|length(split1_range)>3){
    qs=quantile(matrix2vator(up),seq(0,1,0.01),na.rm = T)
    split1_range=c(qs['5%'],qs['50%'],qs['95%'])
  }

  if(min(split1_range)==max(split1_range)){
    split1_range[1]=min(split1_range)-0.000001
    split1_range[length(split1_range)]=max(split1_range)+0.000001
  }
  if(length(split1_range)==2&length(split1_range_cols)!=2){
    split1_range_cols=c(mg_colors[2],mg_colors[1])
  }
  if(length(split1_range)==3&length(split1_range_cols)!=3){
    split1_range_cols=c(mg_colors[2],'white',mg_colors[1])
  }


  data=matrix(NA,ncol = ncol(up),nrow = nrow(up))
  data_type=matrix(NA,ncol = ncol(up),nrow = nrow(up))
  data_cols=matrix(NA,ncol = ncol(up),nrow = nrow(up))
  show_text=matrix(NA,ncol = ncol(up),nrow = nrow(up))
  data_b_cols=matrix('grey',ncol = ncol(up),nrow = nrow(up))
  UpColor <- circlize::colorRamp2(breaks = split1_range, colors = split1_range_cols)
  down_heat=-log10(down+0.000001)
  if(is.null(split2_range)|length(split2_range)==1|length(split2_range)>3){
    qs=quantile(matrix2vator(down_heat),seq(0,1,0.01),na.rm = T)
    split2_range=c(qs['5%'],qs['50%'],qs['95%'])
  }
  if(min(split2_range)==max(split2_range)){
    split2_range[1]=min(split2_range)-0.000001
    split2_range[length(split2_range)]=max(split2_range)+0.000001
  }
  if(length(split2_range)==2&length(split2_range_cols)!=2){
    split2_range_cols=c(mg_colors[3],mg_colors[4])
  }
  if(length(split2_range)==3&length(split2_range_cols)!=3){
    split2_range_cols=c(mg_colors[3],'white',mg_colors[4])
  }


  DnColor <- circlize::colorRamp2(breaks = split2_range, colors = split2_range_cols)


  for(i in 1:(ncol(up)-1)){
    #data_type[i,i]='rect'
    for(j in (i+1):ncol(up)){
      data[i,j]=up[i,j]
      data_type[i,j]=type
      if(down[i,j]==0){
        data[j,i]=-log10(0.000001)
      }else{
        data[j,i]=-log10(down[i,j])
      }
      data_type[j,i]='color'
      data_cols[i,j]=UpColor(up[i,j])
      data_cols[j,i]=DnColor(data[j,i])
      data_b_cols[j,i]=DnColor(data[j,i])
      show_text[j,i]=mg_format_p_value(down[i,j])
    }
  }
  row.names(data)=row.names(up)
  colnames(data)=colnames(up)


  if(is.null(title)){
    title=''
  }


  p1=mg_plot_hetmap_polygon(data,data_type,data_cols,data_b_cols,title,show_text = show_text)
  col_fun = circlize::colorRamp2(split2_range, split2_range_cols)
  lgd <- Legend(title = leg_title2,
                col_fun = col_fun,
                at = seq(min(split2_range),max(split2_range),(max(split2_range)-min(split2_range))/4),
                labels = signif(seq(min(split2_range),max(split2_range),(max(split2_range)-min(split2_range))/4),2),
                direction = "horizontal"
  )
  col_fun2 = circlize::colorRamp2(split1_range, split1_range_cols)

  lgd2 <- Legend(title = leg_title1,
                 col_fun = col_fun2,
                 at = seq(min(split1_range),max(split1_range),(max(split1_range)-min(split1_range))/4),
                 labels = signif(seq(min(split1_range),max(split1_range),(max(split1_range)-min(split1_range))/4),2),
                 direction = "horizontal"
  )

  return(draw(p1, annotation_legend_list = list(lgd,lgd2), annotation_legend_side = "bottom"
              ,heatmap_legend_side = "bottom", merge_legend = TRUE))
}
