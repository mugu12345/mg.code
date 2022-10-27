mg_plot_hetmap_polygon=function(data,data_type=NULL,data_cols=NULL,data_b_cols=NULL,show_text=NULL,title=NULL,border_col='grey',cell_width=NULL,cell_height=NULL){
  draw.pie <- function(x, y, r, init.angle = 90, angle = 180, ...) {
    t <- seq(from = init.angle, to = init.angle + angle, by = sign(angle)) * pi / 180
    grid.polygon(unit.c(x, x + r * cos(t)), unit.c(y, y + r * sin(t)), ...)
  }
  DiagFunc <- function(up,data_type,data_cols,data_b_cols){#circle, square, ellipse, number, shade,color, pie
    library(grid)
    library(ComplexHeatmap)
    function(j, i, x, y, width, height, fill){
      fcol=mg_colors[2]
      bcol='grey'
      tp='pie'
      r=min(width,height)*0.5
      if(!is.null(data_cols)){
        if(nrow(data_cols)>=i&ncol(data_cols)>=j){
          fcol=data_cols[i, j]
        }
      }
      if(!is.null(data_b_cols)){
        if(nrow(data_b_cols)>=i&ncol(data_b_cols)>=j){
          bcol=data_b_cols[i, j]
        }
      }

      if(!is.null(data_type)){
        if(nrow(data_type)>=i&ncol(data_type)>=j){
          tp=data_type[i, j]
        }
      }
      if(!is.null(border_col)&!is.na(border_col)){
        grid.rect(x,y,width = width,height = height,gp = gpar(col=border_col))
      }
      if(!is.na(up[i,j])){
        if(tp=='pie'){
          dt=abs(up[i,j])*360
          if(dt>360){
            dt=360
          }
          draw.pie((x),(y),r = (r),gp=gpar(fill = fcol,col=bcol),angle = dt)
        }else if(tp=='circle'){
          grid.circle(x,y,r = r*up[i,j],gp = gpar(fill = fcol,col=bcol))
        }else if(tp=='color'){
          grid.rect(x,y,width = width,height = height,gp=gpar(fill = fcol,col=NA))
        }else if(tp=='number'){
          grid.text(label=up[i,j],x=x ,y=y,gp = gpar(fill = fcol,col=bcol))
        }else if(tp=='rect_number'){
          #print(fcol)
          grid.rect(x,y,width = width,height = height,gp = gpar(fill = fcol,col=NA))
          grid.text(label=up[i,j],x=x ,y=y,gp = gpar(col='black'))
        }

        if(!is.null(show_text)){
          if(nrow(show_text)>=i & ncol(show_text)>=j){
            txt=show_text[i,j]
            if(!is.na(txt)&txt!=''){
              grid.text(label=txt,x=x,y=y,gp = gpar(fill = NA,col='black'))
            }
          }
        }

      }
    }}

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
  p1 <- Heatmap(data, column_title = title
                , rect_gp = gpar(type = "none")
                , show_heatmap_legend = F
                , cluster_rows = F
                , cluster_columns = F
                ,width = wi
                ,height = hi
                #left_annotation = row_an,
                #top_annotation = col_an,
                ,cell_fun = DiagFunc(data,data_type,data_cols,data_b_cols)
  )
  return(p1)
}
