mg_forestplot=function(HR,ID,lower,upper,p,line_col = "blue",box_col="red",boxsize=0.1,lty.ci=2,lwd.ci=2,ci.vertices.height=0.05,xlab='HR',xlog=F,zero = 1,lineheight='auto',xlim=NULL){
  library(forestplot)
  library("ggplot2")
  p.value <- p
  pairs_CI <- paste(HR,"(",lower, " ~ ",upper, ")", sep = "")
  pairs_name <- ID
  Data_str <- data.frame(pairs_na=pairs_name,pairs_CI=pairs_CI,pairs_p=p.value)
  Data_str <- as.matrix(Data_str)
  Data_str <- rbind(c(NA,"HR","P Value"),Data_str)
  if(xlog&is.null(xlim)){
    xlim=c(min(lower,na.rm=T),max(upper[!is.infinite(upper)],na.rm=T))
  }
  if(is.null(xlim)){
    xlim=c(min(lower,na.rm=T),max(upper[!is.infinite(upper)],na.rm=T))
  }

  if(!is.null(xlim)){
    if(xlog){
      if(xlim[1]<=0){
        xlim[1]=0.01
      }
    }
    lower[which(lower<xlim[1])]=xlim[1]
    upper[which(upper<xlim[1])]=xlim[1]
    HR[which(HR<xlim[1])]=xlim[1]
    lower[which(lower>xlim[2])]=xlim[2]
    upper[which(upper>xlim[2])]=xlim[2]
    HR[which(HR>xlim[2])]=xlim[2]
  }
  mn=min(c(HR,lower,upper),na.rm = T)
  mx=max(c(HR,lower,upper),na.rm = T)
  forest.plot=NULL
  if(mn==mx){
    plot(1:10,1:10,xlab = 'None data return null')
  }else{

    forest.plot=forestplot(Data_str,  #显示的文本
                           c(NA,HR), #误差条的均值(此处为差值的中值)
                           c(NA,lower), #误差条的下界(此处为差值的25%分位数)
                           c(NA,upper), #误差条的上界(此处为差值的75%分位数)
                           zero = zero, #显示y=0的垂直线
                           xlog=xlog, #x轴的坐标不取对数
                           fn.ci_norm = fpDrawCircleCI, #误差条显示方式
                           boxsize = boxsize, ##误差条中的圆心点大小
                           col=fpColors(line = line_col, #误差条的线的颜色
                                        box=box_col), #误差条的圆心点的颜色
                           lty.ci = lty.ci,   # 误差条的线的线型
                           lwd.ci = lwd.ci,   # 误差条的线的宽度
                           ci.vertices.height = ci.vertices.height, # # 误差条末端的长度
                           txt_gp = fpTxtGp(ticks = gpar(cex = 1), xlab = gpar(cex = 1), cex = 1), #文本大小设置
                           lineheight = lineheight, #线的高度
                           xlab=xlab #x轴的标题
    )
  }
  return(forest.plot)
}
