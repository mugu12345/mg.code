mg_plot_lasso=function(fit,cv_fit,lambda=NULL,show_text=T,figLabels=c('A','B')){
  if(is.null(lambda)){
    lmda=cv_fit$lambda.min
  }else{
    lmda=lambda
  }
  fit.coef=fit$beta[(apply(fit$beta,1,function(x){
    return(sum(x!=0))
  })>0),]

  fit.coef=as.matrix(fit.coef)
  colnames(fit.coef)=fit$lambda
  #fit$lambda==cv_fit$lambda
  library(ggplot2)
  dat=data.table::melt(t(as.matrix(fit.coef)))
  dat_z=dat[which(dat$value==0),]
  dat=dat[which(dat$value!=0),]
  dat.sv=rbind()
  for (u in unique(dat_z[,2])) {
    t.z=dat_z[which(dat_z[,2]==u),1]
    t.zx=max(t.z)
    dat.sv=rbind(dat.sv,c(t.zx,u,0))
    t.zn=min(t.z)
    if(t.zx!=t.zn){
      dat.sv=rbind(dat.sv,c(t.zn,u,0))
    }
  }
  colnames(dat.sv)=colnames(dat_z)
  #dat_z=dat_z[dat_z[,2]%in%names(which(fit.coef[,which(fit$lambda==lmda)]!=0)),]
  dat=crbind2DataFrame(rbind(dat,dat.sv))
  mn=min(-log(dat$Var1))
  mx=max(-log(dat$Var1))
  if(show_text){
    mx=(mx-mn)*0.1+mx
  }
  p=ggplot(dat, aes(x=-log(Var1), y=value,colour=Var2))+geom_line()+theme_bw()+theme(legend.position = "none")
  p=p+coord_cartesian(xlim=c(mn, mx))+xlab('-ln(lambda)')+ylab('Coefficients')
  if(show_text){
    fl=fit.coef[which(fit.coef[,which(fit$lambda==lmda)]!=0),ncol(fit.coef)]
    for_label=data.frame(Var1=rep(min(dat$Var1),length(fl)),Var2=names(fl),value=fl)
    p=p+ggrepel::geom_label_repel(
      aes(label = Var2,color=Var2),
      data = for_label,hjust = 0
    )
  }
  p=p+geom_vline(aes(xintercept=-log(lmda)), colour="#BB0000", linetype="dashed")
  p=p+annotate('text',x=-log(lmda),y=min(dat[,3]),label=paste0('λ=',round(lmda,4)))
  tgc=data.frame(lambda=cv_fit$lambda,cvm=cv_fit$cvm,cvup=cv_fit$cvup,cvlo=cv_fit$cvlo,cvsd=cv_fit$cvsd
                 ,col=ifelse(cv_fit$lambda>=cv_fit$lambda.min&cv_fit$lambda<=cv_fit$lambda.1se,ifelse(cv_fit$lambda==lmda,'A','C'),'B'))
  p1=ggplot(tgc, aes(x=log(lambda), y=cvm)) + xlab('ln(lambda)')+ ylab('Parial Likelihood Deviance')+
    geom_errorbar(aes(ymin=cvm-cvsd, ymax=cvm+cvsd)) +
    geom_point(aes(colour=col))
  p1=p1+theme_bw()+theme(legend.position = "none")
  gal=ggpubr::ggarrange(p,p1, ncol = 1, nrow = 2
                        #,align = "hv"
                        ,labels = figLabels)
  return(gal)
}
