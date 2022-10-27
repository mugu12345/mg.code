plot_cor_circle=function(methy_r_all.melt,label_col=NULL,R_col=NULL,P_col=NULL,show_text=NULL){
  library(circlize) 
  circos.clear()  #这个命令用于清空画布，画错时要运行此命令重新再画。
  #整体布局
  circos.par(canvas.xlim =c(-1,1),canvas.ylim = c(-1,1),cell.padding = c(0.01,0,0.01,0)
             #,track.margin=c(0,0)
             ,start.degree = 90
             #,gap.degree=0.01
             ,clock.wise = T)
  fa = methy_r_all.melt$ID
  fa = factor(fa,levels = fa)
  circos.initialize(factors = fa, xlim = c(0,1)) # 初始化
  circos.trackPlotRegion(
    ylim = c(0, 1), track.height = 0.08, bg.border = NA,
    panel.fun = function(x, y) {
      sector.index = get.cell.meta.data('sector.index')
      xlim = get.cell.meta.data('xlim')
      ylim = get.cell.meta.data('ylim')
    } )
  i=1
  for(tc in unique(methy_r_all.melt[,2])){
    inds1=which(methy_r_all.melt[,2]==tc)
    highlight.sector(as.character(methy_r_all.melt$ID[inds1]), track.index = 1,
                     text = tc, niceFacing = F,col = mg_colors[4+i],text.col = 'black')
    i=i+1
  }
  #dim(methy_r_all.melt)
  #cbind(methy_r_all.melt[,1],mg_colors[as.numeric(as.factor(methy_r_all.melt[,1]))])
  if(is.null(label_col)){
    label_col=mg_colors[as.numeric(as.factor(methy_r_all.melt[,1]))]
  }
  if(length(label_col)!=nrow(methy_r_all.melt)){
    label_col=mg_colors[as.numeric(as.factor(methy_r_all.melt[,1]))]
  }
  circos.trackPlotRegion(
    ylim = c(0, 1), track.height = 0.08
    ,bg.border = label_col
    ,cell.padding =c(0,0,0,0)
    ,track.margin=c(0,0)
    , bg.col = label_col,
    panel.fun = function(x, y) {
      sector.index = get.cell.meta.data('sector.index')
      xlim = get.cell.meta.data('xlim')
      ylim = get.cell.meta.data('ylim')
    })
  
  col_fun = colorRamp2(c(-max(abs(methy_r_all.melt$value))
                         ,0,max(abs(methy_r_all.melt$value)))
                       ,c("darkgreen", "white", "darkred"))
  if(is.null(R_col)){
    R_col=col_fun(methy_r_all.melt[,3])
  }
  if(length(R_col)!=nrow(methy_r_all.melt)){
    R_col=col_fun(methy_r_all.melt[,3])
  }

  
  circos.trackPlotRegion(
    ylim = c(0, 1), track.height = 0.1, bg.border = NA, bg.col = R_col,
    panel.fun = function(x, y) {
      sector.index = get.cell.meta.data('sector.index')
      xlim = get.cell.meta.data('xlim')
      ylim = get.cell.meta.data('ylim')
    })
  
  col_fun1 = colorRamp2(c(-log10(0.2),5)
                        ,c("white", "blue"))
  if(is.null(P_col)){
    P_col=col_fun1(-log10(methy_r_all.melt[,4]))
  }
  if(length(P_col)!=nrow(methy_r_all.melt)){
    P_col=col_fun1(-log10(methy_r_all.melt[,4]))
  }
  circos.trackPlotRegion(
    ylim = c(0, 1), track.height = 0.05, bg.border = NA
    , bg.col = P_col,
    panel.fun = function(x, y) {
      sector.index = get.cell.meta.data('sector.index')
      xlim = get.cell.meta.data('xlim')
      ylim = get.cell.meta.data('ylim')
    })
  if(!is.null(show_text)&length(show_text)==nrow(methy_r_all.melt)){
    circos.trackPlotRegion(
      ylim = c(0, 1), track.height = 0.08, bg.border = NA,
      panel.fun = function(x, y) {
        sector.index = get.cell.meta.data('sector.index')
        xlim = get.cell.meta.data('xlim')
        ylim = get.cell.meta.data('ylim')
      } )
    for(i in which(!is.na(show_text))){
      circos.axis(sector.index= methy_r_all.melt$ID[i], direction = "inside"
                  ,labels=show_text[i]
                  ,labels.facing = "clockwise"
                  ,labels.cex=.58, col = 'black',
                  labels.away.percentage=0.1, minor.ticks=0
                  ,major.at=seq(1, length(methy_r_all.melt$ID))
      )
    }
  }
}