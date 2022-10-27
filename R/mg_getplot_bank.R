mg_getplot_bank=function(txt){
  library(ggplot2)
  g.bank=ggplot(data.frame(pca1=1:10,pca2=1:10),aes(x=pca1,y=pca2))+theme_bw()+geom_text(data=data.frame(time=5,surv=5),aes(x=time, y=surv, label=txt),color="black",hjust =0.5)
  g.bank=g.bank+xlab('')+ylab('')+theme(panel.grid =element_blank()) +theme(axis.text = element_blank())+ theme(axis.text.y = element_blank())+ theme(axis.text.x = element_blank())
  return(g.bank)
}
