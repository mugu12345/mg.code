createCoxModel=function(dat,time,event,isStep=F,direction=c("both", "backward", "forward")[1],check=T){
  cls=colnames(dat)
  dat1=cbind(dat,time,event)
  colnames(dat1)=c(paste0('g',1:ncol(dat)),'time','status')
  dat1=as.data.frame(dat1)
  if(ncol(dat)>nrow(dat)&check){
    print('gene count > sample count')
    return(NULL)
  }
  #nas=apply(dat1, 1, function(x){
  #  return(sum(is.na(x)))
  #})
  #dat1=dat1[which(nas==0),]

  fmla <- as.formula(paste0("Surv(time, status) ~",paste0(colnames(dat1)[1:ncol(dat)],collapse = '+')))
  library(survival)
  cox <- coxph(fmla, data = dat1)

  if(isStep){
    tryCatch({
      cox=step(cox,direction =direction)
    },error = function(e) {
      print(conditionMessage(e))
      return(NULL)
    })
  }
  #score=predict(cox,data=dat1)
  sig.genes=cls[as.numeric(gsub('g','',names(cox$coefficients)))]
  fls=c('RiskScore=')
  for(i in 1:length(sig.genes)){
    if(cox$coefficients[i]>0){
      fls=c(fls,'+',round(cox$coefficients[i],3),'*',sig.genes[i])
    }else{
      fls=c(fls,round(cox$coefficients[i],3),'*',sig.genes[i])
    }
  }
  score=predictRiskScore(cox$coefficients,sig.genes,dat)
  return(list(Cox=cox,Score=score,Genes=sig.genes,Coef=cox$coefficients,fmla=paste0(fls,collapse = '')))
}
