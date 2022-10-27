ggplotKMCoxByScore=function(dat,title='Groups',labs=NULL
                            ,palette='npg',show_confint=T,show_median_text=T){
  library(ggplot2)
  library(survival)
  dat=data.frame(Time=dat[,1],Event=dat[,2],Exp=dat[,3])
  #dat=data.frame(Time=tmp[,1],Event=tmp[,2],Exp=tmp[,3])
  g.cox=coxRun(dat)
  res.cut <- survminer::surv_cutpoint(dat, time = "Time", event = "Event",
                                      variables = c("Exp"))
  g2=ggplotKMCox(survminer::surv_categorize(res.cut)
                 ,title=title
                 ,labs=labs
                 ,add_text=paste0('HR=',round(g.cox[2],2)
                                  ,', 95%CI(',round(g.cox[3],2)
                                  ,', ',round(g.cox[4],2),'),cut=',round(res.cut$cutpoint[1,1],2))
                 ,palette=palette
                 ,show_confint=show_confint
                 ,show_median_text=show_median_text)
  return(g2)
}
