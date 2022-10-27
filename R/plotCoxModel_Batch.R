plotCoxModel_Batch=function(riskScore,dat,time,event,cutoff,title='Groups',hetTitle='z-score of expression',hetColor=c('green','black','red'),mks=c(1,3,5)){
  g1=plotRiskScoreModel(riskScore,dat,time,event,cutoff,hetTitle,hetColor)
  dat=data.frame(time,event,ifelse(riskScore>cutoff,'High','Low'))
  dat=dat[order(dat[,3]),]
  cx=coxRun(dat = data.frame(time,event,riskScore))
  txt=paste0('HR=',round(cx[2],2),' 95CI%(',round(cx[3],2),'-',round(cx[4],2),')')
  g3=ggplotKMCox(dat,title=title,labs=unique(dat[,3]),add_text = txt)
  g2=ggplotTimeROC(time,event,riskScore,mks)
  g23=ggpubr::ggarrange(g2,g3, ncol = 1, nrow = 2,heights = c(1,1)
                        #,align = "hv"
                        ,labels = toupper(letters)[2:3])

  gal=ggpubr::ggarrange(g1,g23, ncol = 2, nrow = 1,widths =  c(1.5,1)
                        #,align = "hv"
                        ,labels = c(toupper(letters)[1],''))
  return(gal)
}
