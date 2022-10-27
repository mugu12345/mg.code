mg_plot_circle_AxisTrack=function(data#data=c(list(Name='',at=c(1,2,3...),label=c('a','b','c',...),major=T,minor=T),list(...),list(...))
                              #axis_ids#圆圈分割成多少个对应的ID，需要在那几个里绘制就标哪几个，可以重复
                             #,ats=NULL#与axis_ids长度一样list，list中为每个向量，为NULL则自动绘制全部
                             #,labels=NULL#与ats长度一样list,list中的每个向量与ats一一对应
                             ,loc=c('top_out','top_in','bottom_in','bottom_out','right')[1]#坐标轴位置，矩形内部下方(bottom_in),矩形外部下方(bottom_out),矩形内部上方（top_in）,矩形外部上方(top_out)
                             ,labels.cex=1){
  for(i in 1:length(data)){
    name=data[[i]]['Name'][[1]]
    if(is.null(name)){
      print('Name Not found')
      return(NULL)
    }
    at=data[[i]]['at'][[1]]
    label=data[[i]]['label'][[1]]
    major=data[[i]]['major'][[1]]
    minor=data[[i]]['minor'][[1]]
    if(is.null(major)||!is.logical(major)){
      major=TRUE
    }
    if(is.null(minor)||!is.logical(minor)){
      minor=TRUE
    }
    if(is.null(at)||is.null(label)){
      label=TRUE
      at=NULL
    }else if(length(at)!=length(label)){
      label=TRUE
      at=NULL
    }
    if(loc=='top_out'){
      circos.axis(sector.index = name, major.at = at, labels = label
                  ,major.tick = major,minor.ticks = minor,labels.cex=labels.cex)
    }else if(loc=='top_in'){
      circos.axis(sector.index = name, major.at = at, labels = label
                  ,direction = "inside", labels.facing = "outside"
                  ,major.tick = major,minor.ticks = minor,labels.cex=labels.cex)
    }else if(loc=='bottom_in'){
      circos.axis(sector.index = name, major.at = at, labels = label
                  ,h = "bottom",major.tick = major,minor.ticks = minor,labels.cex=labels.cex)
    }else if(loc=='bottom_out'){
      circos.axis(sector.index = name, major.at = at, labels = label
                  ,h = "bottom", direction = "inside",labels.facing = "reverse.clockwise"
                  ,major.tick = major,minor.ticks = minor,labels.cex=labels.cex)
    }else if(loc=='right'){
      circos.yaxis(side = "right", sector.index =name,labels.cex=labels.cex
                   )
    }else if(loc=='left'){
      circos.yaxis(side = "left", sector.index =name,labels.cex=labels.cex)
    }  
  }
}