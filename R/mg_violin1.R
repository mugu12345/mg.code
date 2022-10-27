mg_violin1=function(data,xangle=0,ylab='value',xlab='',leg.title='Group',test_method='anova',legend.pos='r',melt=F,jitter=T,ylim=NULL){
  library(ggplot2)
  if(melt){
    data_m=data
    colnames(data_m)=c('Group','value')
  }else{
    data_m=reshape2::melt(data)
    colnames(data_m)=c('Group','value')
  }
  if(xangle==0){
    tx=element_text(colour="black",family="Times")
  }else{
    tx=element_text(angle=xangle,hjust = 1,colour="black",family="Times")
  }

  pos='right'
  if(is.null(legend.pos)){
    pos='none'
  }else if(legend.pos=='tr'){
    pos=c(1,1)
  }else if(legend.pos=='br'){
    pos=c(1,0)
  }else if(legend.pos=='tl'){
    pos=c(0,1)
  }else if(legend.pos=='bl'){
    pos=c(0,0)
  }else if(legend.pos=='t'){
    pos='top'
  }else if(legend.pos=='r'){
    pos='right'
  }else if(legend.pos=='b'){
    pos='bottom'
  }

  ct=length(unique(data_m[,1]))

  p1<-ggplot(data_m,aes(x=Group,y=value))+geom_violin(alpha=0.7)
  if(ct<=10){
    p1=p1+ggsci::scale_fill_npg(name=leg.title)
  }else if(ct<=20){
    p1=p1+ggsci::scale_fill_d3(palette = "category20",name=leg.title)
  }else if(ct<=30){
    cbPalette=c(ggsci::pal_npg("nrc", alpha = 0.6)(10),ggsci::pal_d3("category20", alpha = 0.6)(20))
    p1=p1+scale_fill_manual(values=cbPalette[1:ct])
  }else if(ct<=38){
    cbPalette=c(ggsci::pal_npg("nrc", alpha = 0.6)(10)
                ,ggsci::pal_d3("category20", alpha = 0.6)(20)
                ,ggsci::pal_nejm("default", alpha = 0.6)(8))
    p1=p1+scale_fill_manual(values=cbPalette[1:ct])
  }

  if(jitter){
    p1<-p1+geom_jitter(alpha=0.3,col='black',show.legend=FALSE,width = 0.2)
  }

  p1=p1+theme_bw()+geom_boxplot(width=0.2,aes(fill=Group),outlier.shape = NA)
  p1=p1+theme(axis.text.x=tx, #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
              axis.text.y=element_text(family="Times",face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
              axis.title.y=element_text(family="Times",face="plain"), #设置y轴标题的字体属性
              #panel.border = element_blank(),axis.line = element_line(colour = "black"), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
              legend.text=element_text(face="plain", family="Times", colour="black"  #设置图例的子标题的字体属性
              ),
              legend.title=element_text(face="plain", family="Times", colour="black" #设置图例的总标题的字体属性
              ),
              legend.justification=pos, legend.position=pos
              ,legend.background = element_rect(fill = NA, colour = NA)
              #,panel.grid.major = element_blank(),   #不显示网格线
              #panel.grid.minor = element_blank()
  )+ylab(ylab)+xlab(xlab)
  til=''
  if(test_method=='anova'){
    fit <- aov(value~Group, data = data_m)
    pv=summary(fit)[[1]][5][[1]]
    fv=summary(fit)[[1]][4][[1]]
    til=paste0('ANOVA test p=',signif(pv,2))
  }else if (test_method=='rank'){
    fit=kruskal.test(value~Group, data = data_m)
    pv=fit$p.value
    til=paste0('Kruskal-Wallis test p=',signif(pv,2))
  } else {
    fit=wilcox.test(value~Group, data = data_m)
    pv=fit$p.value
    til=paste0('Wilcox test p=',signif(pv,2))
  }
  p1=p1+ggtitle(til)
  if(!is.null(ylim)){
    p1=p1+ylim(ylim)
  }
  return(p1)
}
