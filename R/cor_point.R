# cor_point!
#
# This is an example function named 'Scatter plot of correlation'
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
cor_point=function(x,y,method='Pearson',top_col='#D55E00',right_col='#009E73',ylab='y expression',xlab='x expression'){
  #x=rnorm(200)
  #y=rnorm(200)
  library(ggplot2)
  data=data.frame(x,y)
  colnames(data)=c('wt','mpg')
  til=''
  if(method=='Pearson'){
    cr=cor.test(x,y)
    p=cr$p.value
    r=cr$estimate
    til=paste0('Pearson\'s correlation\nR=',round(r,3),'\nP=',signif(p,3))
  }else{
    cr=cor.test(x,y,method = "spearman")
    p=cr$p.value
    r=cr$estimate
    til=paste0('spearman correlation\nR=',round(r,3),'\nP=',signif(p,3))
  }

  empty <- ggplot()+geom_point(aes(1,1), colour="white") +
    theme(
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
      ,plot.margin=unit(c(0.1, 0.1, 0, 0), "inches")
    )
  empty=empty+geom_text(aes(x=1, y=1, label=til),color="black")

  plot_top <- ggplot(data, aes(wt, fill=top_col)) +
    geom_density(alpha=.5,fill=top_col) +theme_bw()+
    theme(legend.position = "none",
          #plot.background = element_blank(),
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          #panel.background = element_blank(),
          axis.title.x = element_blank(),
          #axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          #axis.text.y = element_blank(),
          axis.ticks.x = element_blank()
          ,plot.margin=unit(c(0.1, 0, 0, 0.1), "inches")
    )

  plot_right <- ggplot(data, aes(mpg, fill=right_col)) +
    geom_density(alpha=.5,fill=right_col) +coord_flip()+theme_bw()+
    theme(legend.position = "none",
          #plot.background = element_blank(),
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          #panel.background = element_blank(),
          #axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          #axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
          ,plot.margin=unit(c(0.01, 0.1, 0.1, 0), "inches")
    )
  #scale_fill_manual(values = c("orange", "purple")) +

  p1=ggplot(data=data, aes(x=wt, y=mpg))+geom_point()+stat_smooth(method="lm")
  p1=p1+theme_bw()
  p1=p1+theme(axis.text.x=element_text(family="Times",face="plain"), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
              axis.text.y=element_text(family="Times",face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
              axis.title.y=element_text(family="Times",face="plain"), #设置y轴标题的字体属性
              #panel.border = element_blank(),
              axis.line = element_line(colour = "black"), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
              legend.text=element_text(face="plain", family="Times", colour="black"),  #设置图例的子标题的字体属性
              legend.title=element_text(face="plain", family="Times", colour="black"), #设置图例的总标题的字体属性
              plot.margin=unit(c(0.01, 0.01, 0.1, 0.1), "inches")
              #,panel.grid.major = element_blank(),   #不显示网格线
              #panel.grid.minor = element_blank()
  )+ylab(ylab)+xlab(xlab)

  pg1=ggpubr::ggarrange(plot_top,p1, ncol = 1, nrow = 2,heights = c(0.3,1),align = "v")
  pg2=ggpubr::ggarrange(empty,plot_right, ncol = 1, nrow = 2,heights = c(0.3,1),align = "v")

  pgal=ggpubr::ggarrange(pg1,pg2, ncol = 2, nrow = 1,widths = c(1,0.3),align = "h")
  return(pgal)
}
