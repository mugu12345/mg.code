mp_plot_circle_track=function(track_ids,bg.border = 'NA'#向量或者单个颜色值
                              ,bg.col = 'NA'#
                              ,ylim = c(0, 10)
                              ,track.height=NULL#track的高度
){
  if(is.null(track.height)){
    track.height=circos.par("track.height")
  }
  min_height=circos.par('cell.padding')[1]+circos.par('cell.padding')[3]+0.001
  if(track.height<min_height){
    track.height=min_height
  }
  circos.trackPlotRegion(factors = track_ids, ylim =ylim, track.height = track.height,
                         bg.border = bg.border,
                         bg.col=bg.col)
}