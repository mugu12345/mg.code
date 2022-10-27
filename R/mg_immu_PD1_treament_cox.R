mg_immu_PD1_treament_cox=function(genes=c('TP53','PDCD1')){
  all.pd1.dat=mg_get_immu_pd1_treament_exp(T,T,T)
  cm.genes=intersect(genes,row.names(all.pd1.dat$CPM))
  if(length(cm.genes)>0){
    g.dat=all.pd1.dat$CPM[match(cm.genes,row.names(all.pd1.dat$CPM)),]
    g.dat.tpm=all.pd1.dat$tpm[match(cm.genes,row.names(all.pd1.dat$tpm)),]
    g.dat.fpkm=all.pd1.dat$fpkm[match(cm.genes,row.names(all.pd1.dat$fpkm)),]
    g.os=all.pd1.dat$clinical$os
    g.ev=all.pd1.dat$clinical$censOS
    cx=cox_batch(g.dat,g.os,g.ev)
    cx.tpm=cox_batch(g.dat.tpm,g.os,g.ev)
    cx.fpkm=cox_batch(g.dat.fpkm,g.os,g.ev)
    return(list(CPM=cx,TPM=cx.tpm,FPKM=cx.fpkm))
  }else{
    print('Not found Genes')
    return(NULL)
  }
}