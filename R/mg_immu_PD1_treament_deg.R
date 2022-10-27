mg_immu_PD1_treament_deg=function(genes=c('TP53','PDCD1'),levels=c('CPM','TPM','FPKM',NULL)[4]){
  all.pd1.dat=mg_get_immu_pd1_treament_exp(T,T,T)
  cm.genes=intersect(genes,row.names(all.pd1.dat$CPM))
  if(length(cm.genes)>0){
    g.dat=all.pd1.dat$CPM[match(cm.genes,row.names(all.pd1.dat$CPM)),]
    g.dat.tpm=all.pd1.dat$tpm[match(cm.genes,row.names(all.pd1.dat$tpm)),]
    g.dat.fpkm=all.pd1.dat$fpkm[match(cm.genes,row.names(all.pd1.dat$fpkm)),]
    brs=as.character(all.pd1.dat$clinical$binaryResponse)
    bcors=as.character(all.pd1.dat$clinical$`Best Confirmed Overall Response`)
    brs.inds=which(!is.na(brs))  
    bcors.inds=which(!is.na(bcors)&bcors!='NE')  
  if(nrow(g.dat)>1){
    fig1=groupViolin(crbind2DataFrame(t(g.dat[,brs.inds])),brs[brs.inds],ylab = 'CPM')
    fig2=groupViolin(crbind2DataFrame(t(g.dat.tpm[,brs.inds])),brs[brs.inds],ylab = 'TPM')
    fig3=groupViolin(crbind2DataFrame(t(g.dat.fpkm[,brs.inds])),brs[brs.inds],ylab = 'FPKM')
    
    fig12=mg_PlotMutiBoxplot(t(crbind2DataFrame(g.dat[,bcors.inds])),group = bcors[bcors.inds],ylab = 'CPM',legend.pos = 'right',test_method = 'anova',yscale = 'log2',size=0.2,add = 'boxplot')
    fig22=mg_PlotMutiBoxplot(t(crbind2DataFrame(g.dat.tpm[,bcors.inds])),group = bcors[bcors.inds],ylab = 'TPM',legend.pos = 'right',test_method = 'anova',yscale = 'log2',size=0.2,add = 'boxplot')
    fig32=mg_PlotMutiBoxplot(t(crbind2DataFrame(g.dat.fpkm[,bcors.inds])),group = bcors[bcors.inds],ylab = 'FPKM',legend.pos = 'right',test_method = 'anova',yscale = 'log2',size=0.2,add = 'boxplot')
    if(!is.na(levels)&levels=='CPM'){
      fig.all=mg_merge_plot(fig1,fig12,ncol = 2,nrow = 1,labels = c('A','B'),widths = c(0.7,1))
    }else if(!is.na(levels)&levels=='TPM'){
      fig.all=mg_merge_plot(fig2,fig22,ncol = 2,nrow = 1,labels = c('A','B'),widths = c(0.7,1))
    }else if(!is.na(levels)&levels=='FPKM'){
      fig.all=mg_merge_plot(fig3,fig32,ncol = 2,nrow = 1,labels = c('A','B'),widths = c(0.7,1))
    }else{
      if(nrow(g.dat)<10){
        figbr=mg_merge_plot(fig1,fig2,fig3,ncol = 3,nrow = 1,common.legend = T)  
        figcors=mg_merge_plot(fig12,fig22,fig32,ncol = 3,nrow = 1,common.legend = T)  
        fig.all=mg_merge_plot(figbr,figcors,nrow = 2,ncol=1)  
      }else{
        figbr=mg_merge_plot(fig1,fig2,fig3,ncol = 1,nrow = 3,common.legend = T)  
        figcors=mg_merge_plot(fig12,fig22,fig32,ncol = 1,nrow = 3,common.legend = T)  
        fig.all=mg_merge_plot(figbr,figcors,nrow = 1,ncol=2,widths = c(0.7,1))  
      }
    }
  }else{
    fig1=mg_violin(data.frame(brs,as.numeric(g.dat[1,]))[brs.inds,],melt = T,ylab = 'CPM')
    fig2=mg_violin(data.frame(brs,as.numeric(g.dat.tpm[1,]))[brs.inds,],melt = T,ylab = 'TPM')
    fig3=mg_violin(data.frame(brs,as.numeric(g.dat.fpkm[1,]))[brs.inds,],melt = T,ylab = 'FPKM')
    
    fig12=mg_violin(data.frame(bcors,as.numeric(g.dat[1,]))[bcors.inds,],melt = T,ylab = 'CPM')
    fig22=mg_violin(data.frame(bcors,as.numeric(g.dat.tpm[1,]))[bcors.inds,],melt = T,ylab = 'TPM')
    fig32=mg_violin(data.frame(bcors,as.numeric(g.dat.fpkm[1,]))[bcors.inds,],melt = T,ylab = 'FPKM')
    if(!is.na(levels)&levels=='CPM'){
      fig.all=mg_merge_plot(fig1,fig12,ncol = 2,nrow = 1,labels = c('A','B'),widths = c(0.7,1))
    }else if(!is.na(levels)&levels=='TPM'){
      fig.all=mg_merge_plot(fig2,fig22,ncol = 2,nrow = 1,labels = c('A','B'),widths = c(0.7,1))
    }else if(!is.na(levels)&levels=='FPKM'){
      fig.all=mg_merge_plot(fig3,fig32,ncol = 2,nrow = 1,labels = c('A','B'),widths = c(0.7,1))
    }else{
      figbr=mg_merge_plot(fig1,fig2,fig3,ncol = 3,nrow = 1,common.legend = T)  
      figcor=mg_merge_plot(fig12,fig22,fig32,ncol = 3,nrow = 1,common.legend = T)  
      fig.all=mg_merge_plot(figbr,figcor,nrow=2,ncol=1)
    }
  }
  return(fig.all)
  }else{
    print('Not found Genes')
    return(NULL)
  }
}