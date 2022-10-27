immu_oncogenic_pathways=function(exp_data){
  baseFolder='source'
  imm.genes=readMatrix(paste0(baseFolder,'/10_oncogenic_pathways_335_genes_pmid_29625050.txt'),row=F)
  all.gs=cbind()
  gnames=c()
  for(g in unique(imm.genes[,3])){
    gs=imm.genes[which(imm.genes[,3]==g),1]
    gs.score=ssGSEAScore_by_genes(exp_data,gs)
    all.gs=cbind(all.gs,gs.score[1,])
    gnames=c(gnames,g)
  }
  all.gs=crbind2DataFrame(all.gs)
  colnames(all.gs)=gnames
  row.names(all.gs)=colnames(exp_data)
  return(all.gs)
}
