mg_CpgAnnoPromoters=function(cpgs){
  load(paste0(MG_Grobal_baseFolder,'/source/genome_info/cpg.450.27.cpg2enstbytss.Rdata'))
  cpg.enst=cpg2enstbytss[cpg2enstbytss[,1]%in%cpgs,]
  load(paste0(MG_Grobal_baseFolder,'/source/gencode.v33.id.tab.RData'))
  dat=cbind(cpg.enst,gencode.v33.id.tab[match(cpg.enst[,2],gencode.v33.id.tab[,1]),-1])
  colnames(dat)=c('CpG','ENST','TSS','ENSG','Symbol','ENST_Type','LncRNA')
  return(dat)
}
