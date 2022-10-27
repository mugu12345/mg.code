mg_immu_PD1_treament_score=function(genes=c('TP53','PDCD1'),levels=c('CPM','TPM','FPKM',NULL)[4],method=c('ssgsea','cox')[1]){
  all.pd1.dat=mg_get_immu_pd1_treament_exp(T,T,T)
  cm.genes=intersect(genes,row.names(all.pd1.dat$CPM))
  g.os=all.pd1.dat$clinical$os
  g.ev=all.pd1.dat$clinical$censOS
  if(length(cm.genes)>0){
    if(!is.na(levels)&levels=='CPM'){
      if(method=='cox'){
        g.dat=all.pd1.dat$CPM[match(cm.genes,row.names(all.pd1.dat$CPM)),]
        cx.m=createCoxModel(dat = t(g.dat),time = g.os,event = g.ev)      
        cpm.score=cx.m$Score
      }else{
        cpm.score=ssGSEAScore_by_genes(all.pd1.dat$CPM,genes = cm.genes)
      }
    }else if(!is.na(levels)&levels=='TPM'){
      if(method=='cox'){
        g.dat.tpm=all.pd1.dat$tpm[match(cm.genes,row.names(all.pd1.dat$tpm)),]
        cx.tpm=createCoxModel(dat = t(g.dat.tpm),time = g.os,event = g.ev)      
        tpm.score=cx.tpm$Score
      }else{
        tpm.score=ssGSEAScore_by_genes(all.pd1.dat$tpm,genes = cm.genes)
      }
    }else if(!is.na(levels)&levels=='FPKM'){
      if(method=='cox'){
        g.dat.fpkm=all.pd1.dat$fpkm[match(cm.genes,row.names(all.pd1.dat$fpkm)),]
        cx.fpkm=createCoxModel(dat = t(g.dat.fpkm),time = g.os,event = g.ev)      
        fpkm.score=cx.fpkm$Score
      }else{
        fpkm.score=ssGSEAScore_by_genes(all.pd1.dat$fpkm,genes = cm.genes)
      }
    }else{
      if(method=='cox'){
        g.dat=all.pd1.dat$CPM[match(cm.genes,row.names(all.pd1.dat$CPM)),]
        g.dat.tpm=all.pd1.dat$tpm[match(cm.genes,row.names(all.pd1.dat$tpm)),]
        g.dat.fpkm=all.pd1.dat$fpkm[match(cm.genes,row.names(all.pd1.dat$fpkm)),]
        cx.m=createCoxModel(dat = t(g.dat),time = g.os,event = g.ev)      
        cpm.score=cx.m$Score
        cx.tpm=createCoxModel(dat = t(g.dat.tpm),time = g.os,event = g.ev)      
        tpm.score=cx.tpm$Score
        cx.fpkm=createCoxModel(dat = t(g.dat.fpkm),time = g.os,event = g.ev)      
        fpkm.score=cx.fpkm$Score
      }else{
        cpm.score=ssGSEAScore_by_genes(all.pd1.dat$CPM,genes = cm.genes)
        tpm.score=ssGSEAScore_by_genes(all.pd1.dat$tpm,genes = cm.genes)
        fpkm.score=ssGSEAScore_by_genes(all.pd1.dat$fpkm,genes = cm.genes)
      }
    }
    brs=as.character(all.pd1.dat$clinical$binaryResponse)
    bcors=as.character(all.pd1.dat$clinical$`Best Confirmed Overall Response`)
    brs.inds=which(!is.na(brs))  
    bcors.inds=which(!is.na(bcors)&bcors!='NE')  
    fig13=mg_plot_cox(os = g.os,event = g.ev,rickscore = as.numeric(cpm.score))
    fig23=mg_plot_cox(os = g.os,event = g.ev,rickscore = as.numeric(tpm.score))
    fig33=mg_plot_cox(os = g.os,event = g.ev,rickscore = as.numeric(fpkm.score))
    mg_merge_plot(fig13,fig23,fig33,ncol = 3,nrow=1)
    if(!is.na(levels)&levels=='CPM'){
      fig1=mg_violin(data.frame(brs,as.numeric(cpm.score))[brs.inds,],melt = T,ylab = 'CPM')
      fig12=mg_violin(data.frame(bcors,as.numeric(cpm.score))[bcors.inds,],melt = T,ylab = 'CPM')
      fig.1=mg_merge_plot(fig1,fig12,ncol = 2,nrow = 1,labels = c('A','B'),widths = c(0.7,1))
      fig13=mg_plot_cox(g.os,g.ev,as.numeric(cpm.score))
      fig.all=mg_merge_plot(fig.1,fig13,nrow=2,ncol=1)
    }else if(!is.na(levels)&levels=='TPM'){
      fig2=mg_violin(data.frame(brs,as.numeric(tpm.score))[brs.inds,],melt = T,ylab = 'TPM')
      fig22=mg_violin(data.frame(bcors,as.numeric(tpm.score))[bcors.inds,],melt = T,ylab = 'TPM')
      fig.1=mg_merge_plot(fig2,fig22,ncol = 2,nrow = 1,labels = c('A','B'),widths = c(0.7,1))
      fig23=mg_plot_cox(g.os,g.ev,as.numeric(tpm.score))
      fig.all=mg_merge_plot(fig.1,fig23,nrow=2,ncol=1)
    }else if(!is.na(levels)&levels=='FPKM'){
      fig3=mg_violin(data.frame(brs,as.numeric(fpkm.score))[brs.inds,],melt = T,ylab = 'FPKM')
      fig32=mg_violin(data.frame(bcors,as.numeric(fpkm.score))[bcors.inds,],melt = T,ylab = 'FPKM')
      fig.1=mg_merge_plot(fig3,fig32,ncol = 2,nrow = 1,labels = c('A','B'),widths = c(0.7,1))
      fig33=mg_plot_cox(g.os,g.ev,as.numeric(fpkm.score))
      fig.all=mg_merge_plot(fig.1,fig33,nrow=2,ncol=1)
    }else{
      fig1=mg_violin(data.frame(brs,as.numeric(cpm.score))[brs.inds,],melt = T,ylab = 'CPM')
      fig2=mg_violin(data.frame(brs,as.numeric(tpm.score))[brs.inds,],melt = T,ylab = 'TPM')
      fig3=mg_violin(data.frame(brs,as.numeric(fpkm.score))[brs.inds,],melt = T,ylab = 'FPKM')
      fig12=mg_violin(data.frame(bcors,as.numeric(cpm.score))[bcors.inds,],melt = T,ylab = 'CPM')
      fig22=mg_violin(data.frame(bcors,as.numeric(tpm.score))[bcors.inds,],melt = T,ylab = 'TPM')
      fig32=mg_violin(data.frame(bcors,as.numeric(fpkm.score))[bcors.inds,],melt = T,ylab = 'FPKM')
      figbr=mg_merge_plot(fig1,fig2,fig3,ncol = 3,nrow = 1,common.legend = T)  
      figcor=mg_merge_plot(fig12,fig22,fig32,ncol = 3,nrow = 1,common.legend = T)  
      fig13=ggplotTimeROC(g.os,g.ev,as.numeric(cpm.score))
      fig23=ggplotTimeROC(g.os,g.ev,as.numeric(tpm.score))
      fig33=ggplotTimeROC(g.os,g.ev,as.numeric(fpkm.score))
      figcox=mg_merge_plot(fig13,fig23,fig33,ncol = 3,nrow=1,labels = c('CPM ROC','TPM ROC','FPKM ROC'))
      fig.all=mg_merge_plot(figbr,figcor,figcox,nrow=3,ncol=1)
    }
    return(fig.all)
    
  }else{
    print('Not found Genes')
    return(NULL)
  }
}
