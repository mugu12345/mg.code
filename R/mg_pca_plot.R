mg_pca_plot=function(data,group,p_size=2,loadings=F,loadings.label=F,frame=T,frame.type='norm',label=F,label.size=3,col=NULL){
  library(ggfortify)
  if(nrow(data)!=length(group)){
    return(mg_getplot_bank('data length not eq group'))
  }
  dat1=crbind2DataFrame(data,full = T)
  dat1=impute::impute.knn(as.matrix(dat1))$data
  pca1=prcomp(dat1)
  ndata=crbind2DataFrame(cbind(dat1,Group=group))
  #pca1<-iris%>%select(-5)%>%prcomp()
  g1=autoplot(pca1,data = ndata,col= 'Group',size=p_size,
              loadings =loadings,loadings.label = loadings.label,
              frame = frame,frame.type=frame.type,
              label = label, label.size = label.size
  )+mg_get_ggplot_theme_bw()
  if(!is.null(col)){
    g1=g1+mg_get_ggsci_col(col)
  }
  return(list(Plot=g1,PCA=pca1))
}
