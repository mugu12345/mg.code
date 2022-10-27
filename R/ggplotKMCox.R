ggplotKMCox=function(dat,title='Groups',labs=NULL,add_text=NULL,palette='npg',show_confint=T,show_median_text=T){
  library(ggplot2)
  library(survival)
  colnames(dat)=c('time','status','groups')
  #sdf<-survdiff(Surv(time,status) ~ groups,data=dat)
  #print((sdf))
  #summary(sdf)
  #p<-pchisq(sdf$chisq,length(sdf$n)-1,lower.tail=FALSE)
  sf<-survival::survfit(Surv(time,status) ~ groups,data=dat)
  surv=survminer::ggsurvplot(sf, data = dat, palette = palette, #jco palette
                             pval = TRUE,surv.median.line='hv'
                             #,conf.int = T
                             ,conf.int.style ='step'
                             , pval.coord=c(0, 0.2), #Add p-value
                             risk.table = TRUE,
                             legend.title = title
                             ,legend.labs = labs
                             ,conf.int=show_confint
  )
  p1=surv$plot+theme_bw()+theme(axis.text.y=element_text(family="Times",face="plain")
                                ,axis.text.x=element_blank()
                                ,axis.title.x=element_blank()
                                ,plot.margin=unit(c(0.2, 0.2, 0, 0.1), "inches")
                                #,axis.title.y=element_blank()
                                ,legend.position=c(1,1), legend.justification=c(1,1)
                                ,legend.background = element_rect(fill = NA, colour = NA)
                                ,legend.title = element_text(family="Times",face="plain")
                                ,legend.text = element_text(family="Times",face="plain"))
  if(show_median_text){
    median_labels=c()
    for(st in unique(surv$data.survplot$strata)){
      st1=surv$data.survplot[which(surv$data.survplot$strata==st),]
      x_m=-1
      if(min(st1$surv)<0.5){
        inds=which(st1$surv==0.5)
        if(length(inds)>0){
          x_m=st1$time[inds[1]]
        }else{
          x_m=max(st1$time[st1$surv>=0.5])
        }
      }
      if(x_m>0){
        median_labels=c(median_labels,round(x_m,1))
      }
    }
    if(length(median_labels)>0){
      txt_median=surv$data.survplot[1:length(median_labels),]
      txt_median[,5]=rep(0.5,length(median_labels))
      txt_median[,1]=median_labels
      txt_median$Text=median_labels
      p1=p1+geom_text(data=txt_median,aes(x=time, y=surv, label=Text),color="black",hjust =1,angle=90,alpha=0.5,vjust=0,nudge_y = -0.01)
    }
  }
  #p1=p1+text()
  #tms=data.frame(Group=tms.gp,value=tms.tps,Attribute=rep(data_m[1,1],length(tms.gp))
  #               ,ymax=rep(max(ylim),length(tms.gp)))
  #p4=p4+geom_text(data=tms,aes(x=Group, y=ymax, label=value),color="black")
  if(is.null(add_text)){
    gp=unique(dat[,3])
    vls=1:length(gp)
    gvls=vls[match(dat[,3],gp)]
    g.cox=coxRun(data.frame(dat[,1],dat[,2],gvls))
    add_text=paste0('HR=',round(g.cox[2],2)
                    ,', 95%CI(',round(g.cox[3],2)
                    ,', ',round(g.cox[4],2),')'
                    ,'\nHR By:',paste0(gp,collapse = '<'))

  }
  text.tb=surv$data.survplot[1,]
  text.tb[1,1]=0
  text.tb[1,5]=0
  text.tb$Text=add_text
  p1=p1+geom_text(data=text.tb,aes(x=time, y=surv, label=Text),color="black",hjust =0)

  p2=surv$table+theme_bw()+theme(axis.text.y=element_text(family="Times",face="plain")
                                 #,axis.text.x=element_blank()
                                 #,axis.title.x=element_blank()
                                 #,axis.title.y=element_blank()
                                 ,plot.margin=unit(c(0, 0.2, 0.2, 0.1), "inches")
                                 ,plot.title=element_blank()
                                 ,legend.position=c(1,1), legend.justification=c(1,1)
                                 #,legend.background = element_rect(fill = NA, colour = NA)
                                 ,legend.title = element_text(family="Times",face="plain")
                                 ,legend.text = element_text(family="Times",face="plain"))

  g2=ggpubr::ggarrange(p1,p2, ncol = 1, nrow = 2,heights = c(1,0.3),align = "v")
  return(g2)
}
