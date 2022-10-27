mg_plot_cox=function(os,event,rickscore){
  library(ggplot2)
  dat=data.frame(Time=os,Event=event,Exp=rickscore)

  #dat=data.frame(Time=tmp[,1],Event=tmp[,2],Exp=tmp[,3])
  g.cox=coxRun(dat)
  cut_list=mg_cutoff_by_maxstat(riskScore=rickscore,time=os,event=event)
  gp=ifelse(rickscore>cut_list$Cutoff,'High','Low')
  #res.cut <- survminer::surv_cutpoint(dat, time = "Time", event = "Event",
  #                                    variables = c("Exp"))
  #survminer::surv_categorize(res.cut)
  p=ggplotKMCox(dat =  data.frame(os,event,gp)
                #,title = paste0(MG_GENE,' in ',tc,' Exp')
                ,add_text = paste0('HR=',round(g.cox[2],2)
                                   ,', 95%CI(',round(g.cox[3],2)
                                   ,', ',round(g.cox[4],2),'),cut=',round(cut_list$Cutoff,2)))
  p2=ggplotTimeROC(os,event,score = rickscore)
  m.g2=ggpubr::ggarrange(p,p2, ncol = 2, nrow = 1,labels = toupper(letters)[1:2])
  return(m.g2)
}
