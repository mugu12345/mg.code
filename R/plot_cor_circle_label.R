plot_cor_circle_label=function(methy_r_all.melt,label_col=NULL,R_mn=-1,R_md=0,R_mx=1
                               ,R_mn_col='darkgreen',R_md_col='white',R_mx_col='darkred',P_mn=0.001,P_mn_col='blue',show_text=NULL){
  library(gridBase)
  library(grid)
  #mg_colors[which(names(cir_ids1)==index)]
  labs=unique(methy_r_all.melt[,1])
  if(is.null(label_col)){
    label_col=mg_colors[1:length(labs)]
  }else if(length(label_col)<length(labs)){
    label_col=mg_colors[1:length(labs)]
  }else{
    label_col=label_col[1:length(labs)]
  }  
  
  lgd_lines = ComplexHeatmap::Legend(at = labs, type = "boxplot", 
                                     legend_gp = grid::gpar(col = label_col, lwd = 2)
                                     ,title_position = "topleft", 
                                     title = "Label")
  
  col_fun1=circlize::colorRamp2(c(R_mn,R_md,R_mx), c(R_mn_col,R_md_col,R_mx_col))
  lgd_links1 =ComplexHeatmap::Legend(at = c(R_mn,R_md,R_mx)
                                    , col_fun = col_fun1, 
                                    title_position = "topleft", title = 'correlation coefficient'
                                    ,direction = "horizontal")
  
  col_fun2=circlize::colorRamp2(c(0,-log10(P_mn)), c('white',P_mn_col))
  lgd_links2 =ComplexHeatmap::Legend(at = c(0, -log10(P_mn))
                                    , col_fun = col_fun2, 
                                    title_position = "topleft", title = '-log10(p value)'
                                    ,direction = "horizontal")
  plot.new()
  circle_size = grid::unit(1, "snpc") # snpc unit gives you a square region
  
  pushViewport(viewport(x = 0, y = 0.5, width = circle_size, height = circle_size,
                        just = c("left", "center")))
  par(omi = gridOMI(), new = TRUE)
  
  plot_cor_circle(methy_r_all.melt,label_col=label_col[match(methy_r_all.melt[,1],labs)]
                  ,R_col=col_fun1(methy_r_all.melt[,3]),P_col=col_fun2(-log10(methy_r_all.melt[,4])),show_text=show_text)
  upViewport()
  lgd_list_horizontal = ComplexHeatmap::packLegend(lgd_lines, lgd_links1,lgd_links2)
  ComplexHeatmap::draw(lgd_list_horizontal, x = circle_size, just = "left")
  
#},error = function(e) {
#  print(conditionMessage(e))
#}, finally = {
#  print('succ')
#})
  
}
