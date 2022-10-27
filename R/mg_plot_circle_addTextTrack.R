mg_plot_circle_addTextTrack=function(data#data=list(a=list(Name='',atx=c(1,2,3...),aty=c(1,2,3...),labels=c('a','b','c',...),facing='facing',size=0.8),list(...),list(...))
                                  ,bg.border = 'NA'#向量或者单个颜色值
                                  ,bg.col = 'NA'#
                                  #,ylim = c(0, 10)
                                  ,track.height=NULL#track的高度
                                  ){
  if(is.null(track.height)){
    track.height=circos.par("track.height")
  }
  min_height=circos.par('cell.padding')[1]+circos.par('cell.padding')[3]+0.001
  if(track.height<min_height){
    track.height=min_height
  }
  all.names=c()
  for(i in 1:length(data)){
    name=data[[i]]['Name'][[1]]
    all.names=c(all.names,name)
  }
  circos.trackPlotRegion(factors = all.names, ylim = c(0, 10), track.height = track.height,
                         bg.border = bg.border,
                         bg.col=bg.col
                         ,panel.fun = function(x, y) {
                           index=get.cell.meta.data("sector.index")
                           #print(index)
                           inds=which(all.names==index)
                           if(length(inds)>0){
                             for(i in inds){
                               atx=data[[i]]['at_x'][[1]]
                               if(is.null(atx)||!is.numeric(atx)){
                                 atx=get.cell.meta.data("xcenter")
                               }
                               #print(atx)
                               aty=data[[i]]['at_y'][[1]]
                               ylim=get.cell.meta.data('ylim')
                               if(is.null(aty)){
                                 aty=rep(0.5,length(atx))
                               }      
                               #print(aty)
                               aty=ylim[1]+aty*(ylim[2] - ylim[1])
                               
                               label=data[[i]]['labels'][[1]]
                               if(is.null(label)){
                                 print(get.cell.meta.data("sector.index"))
                                 label=rep(get.cell.meta.data("sector.index"),length(atx))
                               }
                               if(length(label)!=length(atx)){
                                 indx=1:min(length(label),length(atx))
                                 label=label[indx]
                                 atx=atx[indx]
                                 aty=aty[indx]
                               }
                               
                               facing=data[[i]]['facing'][[1]]
                               if(is.null(facing)){
                                 facing='inside'
                               }
                               cex=data[[i]]['size'][[1]]
                               if(is.null(cex)||!is.numeric(cex)){
                                 cex=1
                               }
                               col=data[[i]]['col'][[1]]
                               if(is.null(col)){
                                 col=par("col")
                               }
                               circos.text(atx, aty
                                           ,labels = label
                                           ,sector.index =get.cell.meta.data("sector.index")
                                           ,facing=facing,niceFacing=T
                                           ,cex = cex
                                           ,col =col)
                             }
                             }
                           }
                           #print(get.cell.meta.data("xcenter"))
                           #circos.text(get.cell.meta.data("xcenter"), 10, get.cell.meta.data("sector.index"))
                         )
}