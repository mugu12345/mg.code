getDotplot=function(ylabel,xValue,pointSize,color,xlab,color_lab='Colour',size_lab='Size'
                    ,low_color='blue',high_color='red',low=NULL,high=NULL,theme_bw=T
                    ){
  library(ggplot2)
  #print(color)
  #print(pointSize)
  psize=T
  if(is.null(pointSize)){
    pointSize=rep(1,length(ylabel))
    psize=F
  }else{
    #print(class(pointSize))
    pointSize=as.numeric(as.character(pointSize))
  }
  #print(pointSize)
  csize=T
  if(is.null(color)){
    color=rep(1,length(ylabel))
    csize=F
  }else{
    color=as.numeric(as.character(color))
  }
  #print(color)
  if(is.null(low)){
    low=min(color,na.rm = T)
  }
  if(is.null(high)){
    high=max(color,na.rm = T)
  }
  
  pathway_data=data.frame(description=ylabel,enrichmentRatio=xValue,size=pointSize,FDR=color)
  print(head(pathway_data))
  pathway_data$description = factor(pathway_data$description, levels=unique(ylabel[length(ylabel):1]))
  if(is.character(xValue)){
    pathway_data$enrichmentRatio = factor(pathway_data$enrichmentRatio, levels=unique(pathway_data$enrichmentRatio))
  }
  
  bubble=ggplot(data = pathway_data, aes(x = enrichmentRatio, y = description)) 
  bubble=bubble+xlab(xlab)
  bubble=bubble+geom_point(aes(size = size,color = FDR))
  bubble=bubble+scale_colour_gradient(low = low_color, high = high_color,name=color_lab,limits=c(low, high))
  if(theme_bw){
    bubble=bubble+ggplot2::theme_bw()
  }
  bubble=bubble+theme(axis.title.y=element_blank()
                                                                ,axis.text.y=element_text(family="Times",face="plain")
                                                                ,axis.text.x=element_text(family="Times",face="plain")
                                                                ,plot.title = element_text(hjust = 0.5,family="Times",face="plain")
                                                                ,axis.title.x=element_text(family="Times",face="plain")
                                                                ,legend.title = element_text(family="Times",face="plain")
                                                                ,legend.text = element_text(family="Times",face="plain"))
  bubble=bubble+guides(size=guide_legend(title=size_lab))
  if(!csize){
    bubble=bubble+guides(color=F)
  }
  if(!psize){
    bubble=bubble+guides(size=F)
  }
  return(bubble)
}

