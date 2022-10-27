mg_plot_calibrate=function(time,events,data,cut.time,timeLabel=c("1-year","3-year",'5-year')){
  #cut.time=c(12,12*3,12*5)
  cal_all=list()
  dat=data
  colnames(dat)=paste0('C',1:ncol(data))
  norm.stat.al=data.frame(time=time,status = events,dat)
  fmla <- as.formula(paste0("Surv(time, status) ~"
                            ,paste0(colnames(dat),collapse = '+')))

  for(i in 1:length(cut.time)){
    f1<-cph(formula = fmla,data=norm.stat.al,x=T,y=T,surv = T
            ,na.action=na.delete,time.inc = cut.time[i])
    cal=get_best_calibrate(f1,cut.time[i])
    cal_all=c(cal_all,list(cal))
  }
  plot(cal_all[[1]],lwd = 2
       ,errbar.col = mg_colors[1]
       ,xlim = c(0,1),ylim= c(0,1),xlab = "Nomogram-prediced OS (%)"
       ,ylab = "Observed OS (%)",col = mg_colors[1]
       ,subtitles = F
  )
  mtext("")
  if(length(cal_all)>1){
    for(i in 2:length(cal_all)){
      plot(cal_all[[i]],lwd = 2,lty = 1,errbar.col = mg_colors[i],xlim = c(0,1),ylim= c(0,1),col = mg_colors[i],add = T)
    }
  }
  abline(0,1, lwd = 2, lty = 3, col = 'black')
  legend("topleft", legend = timeLabel[1:length(cut.time)]
         ,col =mg_colors[1:length(cut.time)],lwd = 2,cex = 1.2,bty = "n")
}
