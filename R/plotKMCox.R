plotKMCox=function(dat,title='Kaplan Meier'){
  library(survival)
  #dat=data.frame(dat1[,1],dat1[,2],ifelse(dat1[,3]>median(dat1[,3]),'H','L'))
  dat=crbind2DataFrame(dat)
  colnames(dat)=c('time','status','groups')
  sdf<-survdiff(Surv(time,status) ~ groups,data=dat)
  print((sdf))
  p<-pchisq(sdf$chisq,length(sdf$n)-1,lower.tail=F)
  sf<-survfit(Surv(time,status) ~ groups,data=dat)
  colKm=rainbow(length(sf$strata))
  plot(sf, mark.time = TRUE,col=colKm,xlab=paste("Survival time in day","\np=",round(p,5))
       ,ylab = "Survival probabilities",main=title)
  legend('topright',paste0(gsub('groups=','',names(sf$strata)),'(N=',sf$n,')'), col = colKm,
         lty = c(1,1, 1, 1),lwd=c(1,1,1,1),merge = TRUE)
  return(p)
}
