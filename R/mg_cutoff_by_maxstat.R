mg_cutoff_by_maxstat=function(riskScore,time,event,smethod="LogRank", pmethod="Lau94",minprop=0.10, maxprop=0.9){
  library(maxstat)
  dataset=data.frame(time=time,status=event,x=riskScore)
  cutoff=maxstat.test(Surv(time, status)~x, data=dataset, smethod=smethod, pmethod=pmethod,minprop=minprop, maxprop=maxprop)
  return(list(Stats=cutoff,Cutoff=cutoff$estimate))
}
