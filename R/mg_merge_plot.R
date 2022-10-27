mg_merge_plot=function(...,ncol = NULL, nrow = NULL,
                       labels = NULL, label.x = 0, label.y = 1, hjust = -0.5,
                       vjust = 1.5, font.label = list(size = 14, color = "black", face ="bold"
                                                      , family = NULL), align = c("none", "h", "v", "hv")
                       ,widths = 1, heights = 1, legend = NULL, common.legend = FALSE){
  ml=list(...)
  if(length(ml)==1){
    if(is.list(ml[[1]])){
      ml=ml[[1]]
    }
  }
  gal=ggpubr::ggarrange(plotlist = ml, ncol = ncol, nrow = nrow
                        ,labels = labels,label.x = label.x, label.y = label.y
                        , hjust = hjust,
                        vjust = vjust, font.label = font.label, align = align
                        ,widths = widths, heights = heights, legend = legend
                        , common.legend = common.legend
  )
  return(gal)
}
