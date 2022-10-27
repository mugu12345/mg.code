mg_lasso_cox_anytimes=function(dat,time,event,nfolds=3,lambda.min=T,show_text=T,times=100,figLabels=c('A','B','C')){
  lasso.all.genes=c()
  lassos_all=list()
  for(i in 1:times){
    tryCatch({
      print(paste0('press:',i,'/',times))
      lassos=mg_lasso_cox(dat,time,event,nfolds,lambda.min,show_text,figLabels = figLabels[2:3])
      gs=paste0(length(lassos$Genes),'=',paste0(sort(lassos$Genes),collapse = '+'))
      lasso.all.genes=c(lasso.all.genes,gs)
      lassos_all=c(lassos_all,list(lassos))
    },error = function(e) {}, finally = {})
  }
  ls.tab=crbind2DataFrame(as.data.frame(table(lasso.all.genes)))
  ls.tab=ls.tab[order(ls.tab[,2]),]
  lst.lasso=lassos_all[[which(lasso.all.genes==ls.tab[nrow(ls.tab),1])[1]]]
  ls.tab[,1]=gsub('=.*','-Tags',ls.tab[,1])
  edpx=paste0('.',1:nrow(ls.tab))
  edpx[!ls.tab[,1]%in%names(which(table(ls.tab[,1])>1))]='...'
  ls.tab[,1]=paste0(ls.tab[,1],edpx)
  fig1=mg_barplot(ls.tab,col=NULL,isFillMutiColour = T,bar_width = 0.8,show_txt = F,ylab = 'Number of lasso')
  gal=ggpubr::ggarrange(fig1,lst.lasso$plot, ncol = 2, nrow = 1,labels = c(figLabels[1],''))
  return(list(LastLasso=lst.lasso,count=table(lasso.all.genes),plot=gal,lasso_all=lassos_all))
}
