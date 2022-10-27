mg_randomForestSRC=function(dat,time,event){

  #library(randomForestSRC)
  dat=crbind2DataFrame(dat)
  dat$days=time
  dat$status=event
  dat=dat[which(apply(dat, 1, function(x){return(sum(is.na(x)))})==0),]

  pbc.obj <- randomForestSRC::rfsrc(Surv(days, status) ~ ., dat, importance = TRUE)
  plot_list=list()
  all.vars=list()
  for(i in 1:3){
    indx=1
    if(i==2){
      indx=2
    }
    vs.pbc <- randomForestSRC::var.select(object = pbc.obj,method = c("md", "vh", "vh.vimp")[i])
    xlab=colnames(vs.pbc$varselect)[indx]
    if(nrow(vs.pbc$varselect)>max(vs.pbc$modelsize,20)){
      vs.pbc$varselect=vs.pbc$varselect[1:max(vs.pbc$modelsize,20),]
    }
    all.vars=c(all.vars,list(vs.pbc$topvars))
    if(class(vs.pbc$varselect)=='numeric'){
      lbs=names(vs.pbc$varselect)
      vls=vs.pbc$varselect
      p_col=rep('unselect',length(vs.pbc$varselect))
      p_col[lbs%in%vs.pbc$topvars]='selected'
    }else{
      lbs=row.names(vs.pbc$varselect)
      vls=vs.pbc$varselect[,indx]
      p_col=rep('unselect',nrow(vs.pbc$varselect))
      p_col[row.names(vs.pbc$varselect)%in%vs.pbc$topvars]='selected'
    }
    gl=mg_barplot_point(labels = lbs
                        ,values = vls
                        ,xlab = xlab
                        ,point_cols = p_col,legend.pos = ifelse(i>1,'tr','br'))
    plot_list=c(plot_list,list(gl))
  }
  gal2=mg_merge_plot(plot_list,nrow = 1,ncol = 3,common.legend = F,labels = c('C','D','E'))
  sel.genes=unique(unlist(all.vars))
  lst.genes=sel.genes
  pbc.obj$importance=pbc.obj$importance[order(pbc.obj$importance,decreasing = T)]
  mx_px=max(match(lst.genes,names(pbc.obj$importance)))
  if(mx_px>20){
    pbc.obj$importance=pbc.obj$importance[match(lst.genes,names(pbc.obj$importance))]
  }
  p_col=rep('unselect',length(pbc.obj$importance))
  p_col[names(pbc.obj$importance)%in%lst.genes]='selected'
  g1=mg_line_plot(1:1000,pbc.obj$err.rate,p_size = 0.3,xlab = 'Number of Trees',ylab = 'Error rate')
  g2=mg_barplot_point(labels = names(pbc.obj$importance)
                      ,values = pbc.obj$importance
                      ,xlab = 'Variable Importance',point_cols = p_col)
  gal1=mg_merge_plot(g1,g2,nrow = 1,ncol = 2,labels = c('A','B'))
  gall.all=mg_merge_plot(gal1,gal2,nrow = 2,ncol = 1)
  return(list(plot=gall.all,GenesList=all.vars,Model=vs.pbc))
}
