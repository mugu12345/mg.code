compareRiskModal=function(os,ev,risks,max_time){
  library('survRM2')
  library('Hmisc')
  require(survival)
  library(survcomp)
  library(prodlim)

  # Parameters for RMS
  tau <-max_time # Restriction point
  n.grid <-100 # For plotting purposes
  # Set up data using age as the marker
  OS <-Surv(os,ev)
  marker.pp<-seq(from=0,to=1,length=n.grid)
  mdat=cbind()
  c.all=list()
  c.indexs=c()
  for(i in 1:ncol(risks)){
    marker <-as.numeric(risks[,i])
    marker.qq<-quantile(marker,marker.pp)
    fitdat.df<-data.frame(marker=marker)
    newdat.df<-data.frame(marker=marker.qq)
    # Calculations
    cox.model1<-coxph(OS~marker,data=fitdat.df)
    rms.calc <-summary(survfit(cox.model1,newdata=newdat.df),rmean=tau)
    rms.mean <-rms.calc$table[,"*rmean"]
    mdat=cbind(mdat,rms.mean)
    c1=concordance.index(x=as.numeric(risks[,i]), surv.time=os, surv.event=ev,
                         method="noether")
    c.all=c(c.all,list(c1))
    c.indexs=c(c.indexs,paste0(colnames(risks)[i],':',round(c1$c.index,2),',95%CI(',round(c1$lower,2),'-',round(c1$upper,2),'),p=',signif(c1$p.value,2)))
  }

  mt=ceiling(max(mdat))
  # RMS Curve
  cls=rainbow(ncol(risks))
  if(ncol(risks)>1){
    plot(marker.pp,as.numeric(mdat[,1]),pch=20,col = cls[1], cex=0.5,ylim=c(0,mt),xlab="",ylab="")
    if(ncol(risks)>2){
      for(i in 2:(ncol(risks)-1)){
        par(new=TRUE)
        plot(marker.pp,as.numeric(mdat[,i]),pch=20,col = cls[i]
             , cex=0.7,ylim=c(0,mt),xlab="",ylab="")
      }
    }
    par(new=TRUE)
    plot(marker.pp,as.numeric(mdat[,ncol(risks)]),pch=20,col = cls[ncol(risks)]
         , cex=0.5,ylim=c(0,mt),xlab="Percentile of Score",ylab="RMS")
  }else{
    plot(marker.pp,as.numeric(mdat[,1]),pch=20,col = "red", cex=0.5,ylim=c(0,mt),xlab="Percentile of Score",ylab="RMS")
  }
  legend("topright",   c.indexs, pch=20,col=cls, title= "C-index", inset = .05,cex = 0.7)
  #print(c.all)
  if(ncol(risks)>1){
    all.cp=c()
    for(i in 1:(ncol(risks)-1)){
      c1=concordance.index(x=risks[,i], surv.time=os, surv.event=ev,
                           method="noether")
      for(j in (i+1):ncol(risks)){
        c2=concordance.index(x=risks[,j], surv.time=os, surv.event=ev,
                             method="noether")
        p=min(cindex.comp(c1, c2)$p.value,cindex.comp(c2, c1)$p.value)
        all.cp=c(all.cp,paste0(colnames(risks)[i],'-vs-',colnames(risks)[j],':p=',signif(p,2)))
      }
      legend("bottomleft",   all.cp, title= "Compare C-index", inset = .05,cex = 0.7)
    }
  }
  #detach('package:survRM2')
  #detach('package:Hmisc')
  #detach('package:survival')
  #detach('package:survcomp')
  #detach('package:prodlim')
}
#compareRiskModal(data$SURVIVAL_TIM,data$STATUS,data.frame(IPV=data$IPVHCC,MOD=data$Risk),5*365)
