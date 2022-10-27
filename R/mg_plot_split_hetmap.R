mg_plot_split_hetmap=function(split1,spli2,split1_range=NULL,split1_range_cols=NULL,show_text1=NULL,leg_title1='Panel_top',leg_title2='Panel_bottom'
                              ,split2_range=NULL,split2_range_cols=NULL,show_text2=NULL
                              ,title=NULL,cell_width=NULL,cell_height=NULL
                              ,type=c('bt','lf')[1]
                              ,left_annotation=NULL
                              ,top_annotation=NULL
){#type
  library(grid)
  library(ComplexHeatmap)
  up=split1
  down=spli2
  #print(nrow(up))
  if(is.null(split1_range)|length(split1_range)==1|length(split1_range)>3){
    qs=quantile(matrix2vator(up),seq(0,1,0.01))
    split1_range=c(qs['5%'],qs['50%'],qs['95%'])
  }
  if(is.null(split2_range)|length(split2_range)==1|length(split2_range)>3){
    qs=quantile(matrix2vator(down),seq(0,1,0.01))
    split2_range=c(qs['5%'],qs['50%'],qs['95%'])
  }
  if(min(split1_range)==max(split1_range)){
    split1_range[1]=min(split1_range)-0.000001
    split1_range[length(split1_range)]=max(split1_range)+0.000001
  }
  if(min(split2_range)==max(split2_range)){
    split2_range[1]=min(split2_range)-0.000001
    split2_range[length(split2_range)]=max(split2_range)+0.000001
  }

  if(length(split1_range)==2&length(split1_range_cols)!=2){
    split1_range_cols=c(mg_colors[3],mg_colors[4])
  }
  if(length(split1_range)==3&length(split1_range_cols)!=3){
    split1_range_cols=c(mg_colors[3],'white',mg_colors[4])
  }
  if(length(split2_range)==2&length(split2_range_cols)!=2){
    split2_range_cols=c(mg_colors[1],mg_colors[2])
  }
  if(length(split2_range)==3&length(split2_range_cols)!=3){
    split2_range_cols=c(mg_colors[1],'white',mg_colors[2])
  }

  UpColor <- circlize::colorRamp2(breaks = split1_range, colors = split1_range_cols)
  DnColor <- circlize::colorRamp2(breaks = split2_range, colors = split2_range_cols)

  DiagFunc <- function(up, down,show_text1,show_text2){
    function(j, i, x, y, width, height, fill){
      if(type=='bt'){
        grid.polygon(unit.c(x - 0.5*width, x - 0.5*width, x + 0.5*width),
                     unit.c(y - 0.5*height, y + 0.5*height, y - 0.5*height),
                     gp = gpar(fill = DnColor(down[i, j]), col = "grey"))

        grid.polygon(unit.c(x + 0.5*width, x + 0.5*width, x - 0.5*width),
                     unit.c(y + 0.5*height, y - 0.5*height, y + 0.5*height),
                     gp = gpar(fill = UpColor(up[i, j]), col = "grey"))
        if(!is.null(show_text1)){
          if(nrow(show_text1)>=i&ncol(show_text1)>=j){
            txt=show_text1[i,j]
            if(!is.na(txt)&txt!=''){
              grid.text(label=txt,x=(x + 0.5*width),
                        y=(y+ 0.5*height),just = c('right','top'))
            }
          }
        }
        if(!is.null(show_text2)){
          if(nrow(show_text2)>=i&ncol(show_text2)>=j){
            txt=show_text2[i,j]
            if(!is.na(txt)&txt!=''){
              grid.text(label=txt,x=(x-0.5*width),
                        y=(y-0.5*height),just = c('left','bottom'))
            }
          }
        }
      }else{
        grid.polygon(unit.c(x - 0.5*width, x + 0.5*width, x + 0.5*width),
                     unit.c(y - 0.5*height, y + 0.5*height, y - 0.5*height),
                     gp = gpar(fill = DnColor(down[i, j]), col = "grey"))

        grid.polygon(unit.c(x + 0.5*width, x - 0.5*width, x - 0.5*width),
                     unit.c(y + 0.5*height, y - 0.5*height, y + 0.5*height),
                     gp = gpar(fill = UpColor(up[i, j]), col = "grey"))

        if(!is.null(show_text1)){
          if(nrow(show_text1)>=i&ncol(show_text1)>=j){
            txt=show_text1[i,j]
            if(!is.na(txt)&txt!=''){
              grid.text(label=txt,x=(x - 0.5*width),
                        y=(y+ 0.5*height),just = c('left','top'))
            }
          }
        }
        if(!is.null(show_text2)){
          if(nrow(show_text2)>=i&ncol(show_text2)>=j){
            txt=show_text2[i,j]
            if(!is.na(txt)&txt!=''){
              grid.text(label=txt,x=(x + 0.5*width),
                        y=(y- 0.5*height),just = c('right','bottom'))
            }
          }
        }
      }
    }
  }

  if(is.null(title)){
    title=''
  }

  wi=NULL
  hi=NULL
  if(!is.null(cell_width)){
    wi=cell_width*ncol(up)
  }
  if(!is.null(cell_height)){
    hi=cell_height*nrow(up)
  }
  p1 <- Heatmap(up, column_title = title
                , rect_gp = gpar(type = "none")
                , show_heatmap_legend = F
                , cluster_rows = F
                , cluster_columns = F
                ,width = wi
                ,height = hi
                ,left_annotation = left_annotation,
                top_annotation = top_annotation
                ,cell_fun = DiagFunc(up = up, down = down,show_text1 = show_text1,show_text2=show_text2)
  )
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

  return(ComplexHeatmap::draw(p1, annotation_legend_list = list(lgd,lgd2)
                              , annotation_legend_side = "bottom"
                              ,heatmap_legend_side = "bottom"
                              , merge_legend = TRUE))
}
