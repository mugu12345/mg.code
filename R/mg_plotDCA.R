mg_plotDCA=function(status,fmlas,modelNames,data){
  set.seed(123)
  all.mod=list()
  for(i in 1:length(fmlas)){
    fmla <- as.formula(paste0("status~",fmlas[i]))
    model<-rmda::decision_curve(fmla,
                                data=data,
                                bootstraps=500)
    all.mod=c(all.mod,list(model))
  }
  rmda::plot_decision_curve(all.mod,
                            curve.names=modelNames,
                            xlim=c(0,1),legend.position="topright",
                            confidence.intervals=FALSE)
}
